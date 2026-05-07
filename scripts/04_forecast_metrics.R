# ==============================================================================
# SCRIPT 04: FORECAST EVALUATION METRICS
# ==============================================================================
# Loads all nowcast .rds results, maps each nowcast to the next target not yet
# published at the forecast date, and computes RMSE, MAE, R2 for each
# (Model x Country x Block x Sample) combination.
#
# Samples:
#   - all_vintages: all pseudo-real-time forecast dates
#   - last_before_release: last available forecast for each target_date
#
# Outputs: summary table + wide comparison table + optional errors by date.
# ==============================================================================

# ---- 0. Environment and Processed data ----
if (!exists("pseudo_real_time")) source("_setup.R")

data_processed_all <- readRDS(here("data/processed/data_full_treatments.rds"))

cat("\n======================================================\n")
cat(">>> STARTING FORECAST METRICS PIPELINE\n")
cat("======================================================\n")


# ---- 1. Metric functions ----
rmse <- function(actual, forecast) {
  sqrt(mean((actual - forecast)^2, na.rm = TRUE))
}

mae <- function(actual, forecast) {
  mean(abs(actual - forecast), na.rm = TRUE)
}

r2 <- function(actual, forecast) {
  ss_res <- sum((actual - forecast)^2, na.rm = TRUE)
  ss_tot <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
  if (ss_tot == 0) return(NA_real_)
  1 - ss_res / ss_tot
}

# ---- 2. Load results and compute metrics ----
metrics_list <- list()
errors_list  <- list()

for (model_name in list_models) {
  for (country in list_countries) {
    for (block in list_blocks) {
      
      file_path <- here(
        "data/processed",
        paste0("nowcast_", model_name, "_", tolower(country), "_", tolower(block), ".rds")
      )
      
      if (!file.exists(file_path)) {
        warning(sprintf("[SKIP] File not found: %s", file_path))
        next
      }
      
      nowcast_df <- readRDS(file_path)
      
      # Only successful nowcasts are kept
      # (For instance, there is a "burn-in" period for the factor midas: at the 
      # start, there are more parameters than observable, so the model cannot work)
      nowcast_ok <- nowcast_df[
        nowcast_df$status == "OK" & !is.na(nowcast_df$nowcast_value),
      ]
      
      if (nrow(nowcast_ok) == 0) {
        warning(sprintf("[SKIP] No valid nowcasts for %s - %s - %s",
                        model_name, country, block))
        next
      }
      
      nowcast_ok$publication_date <- as.Date(nowcast_ok$publication_date)
      
      # Load publication delays
      df_publi_delay <- readxl::read_excel(
        here("data/publication_delay.xlsx"),
        sheet = country
      )
      colnames(df_publi_delay) <- trimws(colnames(df_publi_delay))
      
      # stationary target data 
      df_statio <- data_processed_all$statio[[country]][[block]][-(1:3), ]
      
      if (block == "Expenditures" &&
          "Sovereign credit rating revisions" %in% colnames(df_statio)) {
        df_statio[["Sovereign credit rating revisions"]] <- NULL
      }
      
      # Alignement of dates
      # "Economic" target = depends on publication date
      # but in the raw data, target is reported at the very beginning of the next quarter
      # In short: we take the very next unpublished target, as long as it is not published
      # Ex: 20 June: Q1 is published, Q2 and Q3 are not, so the targeted value is the Q2 one.
      actual_targets <- df_statio[!is.na(df_statio[[2]]), c(1, 2)] # "Real" data
      colnames(actual_targets) <- c("target_date", "actual")
      actual_targets$target_date <- as.Date(actual_targets$target_date)
      actual_targets$actual <- as.numeric(actual_targets$actual)
      
      # Compute exact target publication dates 
      target_var <- colnames(df_statio)[2]
      month_delay <- as.numeric(df_publi_delay[2, target_var])
      day_delay   <- as.numeric(df_publi_delay[3, target_var])
      
      target_pubs <- as.Date(date_modifyer(
        actual_targets$target_date,
        month = month_delay,
        days  = day_delay
      ))
      
      if (length(target_pubs) != nrow(actual_targets)) {
        warning(sprintf("[SKIP] Target length mismatch for %s - %s",
                        country, block))
        next
      }
      
      actual_targets$target_pub_date <- target_pubs
      
      # Map each forecast date to the next unpublished target 
      nowcast_ok$actual <- NA_real_
      nowcast_ok$target_date <- as.Date(NA)
      nowcast_ok$target_pub_date <- as.Date(NA)
      
      for (i in seq_len(nrow(nowcast_ok))) {
        pub_date <- nowcast_ok$publication_date[i]
        
        # Last target known as of the forecast date
        past_pubs <- which(actual_targets$target_pub_date <= pub_date)
        
        if (length(past_pubs) > 0) {
          last_obs_idx <- max(past_pubs)
          
          # Forecast target: first target not yet published as of pub_date
          next_target_idx <- last_obs_idx + 1
          
          if (next_target_idx <= nrow(actual_targets)) {
            nowcast_ok$actual[i] <- actual_targets$actual[next_target_idx]
            nowcast_ok$target_date[i] <- actual_targets$target_date[next_target_idx]
            nowcast_ok$target_pub_date[i] <- actual_targets$target_pub_date[next_target_idx]
          }
        }
      }
      
      # Keep only nowcasts with matched actual target
      merged <- nowcast_ok[!is.na(nowcast_ok$actual), ]
      
      if (nrow(merged) < 2) {
        warning(sprintf("[SKIP] Insufficient matched obs for %s - %s - %s (n=%d)",
                        model_name, country, block, nrow(merged)))
        next
      }
      
      # Evaluation samples
      # First, we consider all the nowcasts (averaging)
      merged_all <- merged
      
      # Second, we only consider the last nowcast before the true value is released
      # The goal is to compare the average performance of the model throughout the 
      # whole "information flow" process VS only considering the most informed nowcast.
      merged_last <- merged |>
        dplyr::group_by(target_date) |>
        dplyr::slice_max(publication_date, n = 1, with_ties = FALSE) |>
        dplyr::ungroup()
      
      samples <- list(
        all_vintages = merged_all,
        last_before_release = merged_last
      )
      
      # Metrics for both samples
      for (sample_name in names(samples)) {
        
        sample_df <- samples[[sample_name]]
        
        metrics_list[[length(metrics_list) + 1]] <- data.frame(
          Model   = model_name,
          Country = country,
          Block   = block,
          Sample  = sample_name,
          N       = nrow(sample_df),
          RMSE    = round(rmse(sample_df$actual, sample_df$nowcast_value), 4),
          MAE     = round(mae(sample_df$actual,  sample_df$nowcast_value), 4),
          R2      = round(r2(sample_df$actual,   sample_df$nowcast_value), 4),
          stringsAsFactors = FALSE
        )
        
        cat(sprintf(
          "[OK] %s | %s | %s | %s  ->  RMSE=%.4f  MAE=%.4f  R2=%.4f  (N=%d)\n",
          model_name, country, block, sample_name,
          metrics_list[[length(metrics_list)]]$RMSE,
          metrics_list[[length(metrics_list)]]$MAE,
          metrics_list[[length(metrics_list)]]$R2,
          nrow(sample_df)
        ))
      }
      
      # Debug
      errors_tmp <- merged_all[, c(
        "publication_date",
        "target_date",
        "target_pub_date",
        "nowcast_value",
        "actual"
      )]
      
      errors_tmp$Model <- model_name
      errors_tmp$Country <- country
      errors_tmp$Block <- block
      errors_tmp$error <- errors_tmp$actual - errors_tmp$nowcast_value
      errors_tmp$abs_error <- abs(errors_tmp$error)
      errors_tmp$sq_error <- errors_tmp$error^2
      
      errors_list[[length(errors_list) + 1]] <- errors_tmp
    }
  }
}

# ---- 3. Assemble summary table ----
if (length(metrics_list) == 0) {
  stop("No metrics could be computed. Check that .rds files exist and contain valid nowcasts.")
}

metrics_summary <- do.call(rbind, metrics_list)
rownames(metrics_summary) <- NULL

# ---- 5. User's logs  ----
cat("\n======================================================\n")
cat("FORECAST EVALUATION SUMMARY\n")
cat("======================================================\n")
print(metrics_summary, row.names = FALSE)

# Wide comparison table
make_wide <- function(df, metric) {
  df_sub <- df[, c("Sample", "Country", "Block", "Model", metric)]
  colnames(df_sub)[5] <- "value"
  
  df_wide <- tidyr::pivot_wider(
    df_sub,
    names_from = "Model",
    values_from = "value"
  )
  
  df_wide$Metric <- metric
  
  df_wide <- df_wide[, c(
    "Sample",
    "Metric",
    "Country",
    "Block",
    setdiff(colnames(df_wide), c("Sample", "Metric", "Country", "Block"))
  )]
  
  return(df_wide)
}

comparison_wide <- dplyr::bind_rows(
  make_wide(metrics_summary, "RMSE"),
  make_wide(metrics_summary, "MAE"),
  make_wide(metrics_summary, "R2")
)

cat("\n--- Wide comparison (models as columns) ---\n")
print(comparison_wide, row.names = FALSE)

# ---- 6. Export metrics ----
metrics_file <- here(
  "outputs/tables",
  paste0("forecast_metrics_", format(Sys.Date(), "%Y%m%d"), ".csv")
)

comparison_file <- here(
  "outputs/tables",
  paste0("forecast_metrics_wide_", format(Sys.Date(), "%Y%m%d"), ".csv")
)

write.csv(metrics_summary, metrics_file, row.names = FALSE)
write.csv(comparison_wide, comparison_file, row.names = FALSE)

# ---- 7. Export errors by date ----
if (length(errors_list) > 0) {
  
  errors_by_date <- do.call(rbind, errors_list)
  rownames(errors_by_date) <- NULL
  
  errors_file <- here(
    "outputs/tables",
    paste0("forecast_errors_by_date_", format(Sys.Date(), "%Y%m%d"), ".csv")
  )
  
  write.csv(errors_by_date, errors_file, row.names = FALSE)
  
} else {
  errors_file <- NA_character_
}

cat("\n======================================================\n")
cat("Metrics saved to:\n")
cat(" -", metrics_file, "\n")
cat(" -", comparison_file, "\n")

if (!is.na(errors_file)) {
  cat(" -", errors_file, "\n")
}

cat("======================================================\n")