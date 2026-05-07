# ==============================================================================
# SCRIPT 02: NOWCASTING ESTIMATION
# ==============================================================================
# This script performs the pseudo real-time nowcasting for each 
# country, block and model.
# Results are saved as .rds files and an execution log is generated.
# Results are saved in block x model x country rds but also in one "global" rds.

# ---- 1. Load cleaned data ----
data_processed_all <- readRDS(here("data/processed/data_full_treatments.rds"))
countries_statio   <- data_processed_all$statio

cat("\n======================================================\n")
cat(">>> STARTING NOWCASTING PIPELINE\n")
cat("======================================================\n")



# Factor-MIDAS parameters
kmax_factor         <- 4
lags_factors_factor <- 2

# sg-LASSO-MIDAS parameters
x_lags_sgl          <- 2
y_lags_sgl          <- 1
legendre_degree_sgl <- 0
gamma_sgl           <- 0.5
K_sgl               <- NULL
l_sgl               <- 2
penalize_y_lags_sgl <- FALSE

# ---- 3. Initialize execution log ----
nowcast_log <- data.frame(
  Model     = character(),
  Country   = character(),
  Block     = character(),
  Status    = character(), 
  Message   = character(),
  Nb_obs    = numeric(),
  Timestamp = character(),
  stringsAsFactors = FALSE
)
all_nowcast_results <- list()

# ---- 4. Main nowcasting loop ----
cat(">>> STARTING GLOBAL NOWCASTING PIPELINE\n")

for (model_name in list_models) {
  
  cat("\n======================================================\n")
  cat(">>> RUNNING MODEL:", model_name, "\n")
  cat("======================================================\n")
  
  for (country in list_countries) {
    
    # Load specific publication delays for the current country
    df_publi_delay <- read_excel(here("data/publication_delay.xlsx"), sheet = country)
    colnames(df_publi_delay) <- trimws(colnames(df_publi_delay))
    
    for (block in list_blocks) {
      
      current_time <- format(Sys.time(), "%H:%M:%S")
      cat(sprintf("\n--- Processing: %s | %s | %s [%s] ---\n",
                  model_name, country, block, current_time))
      
      # --- Step A: Data extraction + security checks ---
      df_statio <- countries_statio[[country]][[block]]
      
      if (is.null(df_statio)) {
        msg <- "Data is NULL for this country/block"
        warning(sprintf("!!! [SKIP] %s - %s - %s: %s",
                        model_name, country, block, msg))
        
        nowcast_log <- rbind(nowcast_log, data.frame(
          Model = model_name,
          Country = country,
          Block = block,
          Status = "ERROR", 
          Message = msg,
          Nb_obs = 0,
          Timestamp = current_time,
          stringsAsFactors = FALSE
        ))
        next 
      }
      
      # --- Step B: Pre-estimation cleaning ---
      # Trim the first quarter/3 months to handle stationarization lags
      df_statio <- df_statio[-(1:3), ]
      
      # Generate the ragged-edge dataset 
      ragged_df <- ragged_edge_dataset(df_statio, df_publi_delay)
      
      # Country-specific adjustments
      if ("Sovereign credit rating revisions" %in% colnames(ragged_df)) {
        ragged_df[["Sovereign credit rating revisions"]] <- NULL
      }
      
      # --- Step C: Pseudo Real-Time estimation ---
      estimation_res <- try({
        
        if (model_name == "factor_midas") {
          
          pseudo_real_time(
            df_ragged    = ragged_df,
            start_date   = start_nowcast,
            method       = window_method,
            n_window     = n_window,
            model        = "factor_midas",
            kmax         = kmax_factor,
            lags_factors = lags_factors_factor
          )
          
        } else if (model_name == "sg_lasso") {
          
          pseudo_real_time(
            df_ragged          = ragged_df,
            start_date         = start_nowcast,
            method             = "expanding",
            model              = "sg_lasso",
            x_lags             = x_lags_sgl,
            y_lags             = y_lags_sgl,
            legendre_degree    = legendre_degree_sgl,
            gamma              = gamma_sgl,
            K                  = K_sgl,
            l                  = l_sgl,
            penalize_y_lags    = penalize_y_lags_sgl
          )
          
        } else if (model_name %in% c("ar1", "rw")) {
          
          pseudo_real_time(
            df_ragged          = ragged_df,
            start_date         = start_nowcast,
            method             = window_method,
            n_window           = n_window,
            model              = model_name
          )
          
        } else {
          stop("Unknown model: ", model_name)
        }
        
      }, silent = TRUE)
      
      # --- Step D: Log Recording & Saving ---
      if (inherits(estimation_res, "try-error")) {
        
        err_msg <- attr(estimation_res, "condition")$message
        warning(sprintf("!!! [FAIL] %s - %s - %s: %s",
                        model_name, country, block, err_msg))
        
        nowcast_log <- rbind(nowcast_log, data.frame(
          Model = model_name,
          Country = country,
          Block = block,
          Status = "ERROR", 
          Message = err_msg,
          Nb_obs = nrow(df_statio),
          Timestamp = current_time,
          stringsAsFactors = FALSE
        ))
        
      } else {
        
        cat(sprintf(">>> [OK] Success for %s - %s - %s\n",
                    model_name, country, block))
        
        # Add identifiers directly in the result dataframe
        estimation_res$model <- model_name
        estimation_res$country <- country
        estimation_res$block <- block
        
        estimation_res <- estimation_res %>%
          select(model, country, block, everything())
        
        # Store result in global container
        combo_id <- paste(model_name, country, block, sep = "_")
        all_nowcast_results[[combo_id]] <- estimation_res
        
        # Save raw results as .rds
        file_name_rds <- paste0(
          "nowcast_",
          model_name, "_",
          tolower(country), "_",
          tolower(block),
          ".rds"
        )
        
        saveRDS(estimation_res, here("data/processed", file_name_rds))
        
      
        file_name_csv <- paste0(
          "nowcast_",
          model_name, "_",
          tolower(country), "_",
          tolower(block),
          ".csv"
        )
        
        write.csv(
          estimation_res,
          here("outputs/logs", file_name_csv),
          row.names = FALSE
        )
        
        nowcast_log <- rbind(nowcast_log, data.frame(
          Model = model_name,
          Country = country,
          Block = block,
          Status = "SUCCESS", 
          Message = "Estimation completed",
          Nb_obs = nrow(df_statio),
          Timestamp = current_time,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
}

# ---- 5. Save global nowcast results ----
if (length(all_nowcast_results) > 0) {
  
  all_nowcast_df <- dplyr::bind_rows(all_nowcast_results)
  
  saveRDS(
    all_nowcast_df,
    here("data/processed/nowcast_all_results.rds")
  )
  
  cat("Global nowcast results saved in: data/processed/nowcast_all_results.rds\n")
  
} else {
  
  warning("No successful nowcast result to aggregate.")
}

# Save independent results
log_file_name <- paste0("nowcast_log_", format(Sys.Date(), "%Y%m%d"), ".csv")

write.csv(
  nowcast_log,
  here("outputs/logs", log_file_name),
  row.names = FALSE
)

cat("\n======================================================\n")
cat("*** GLOBAL NOWCASTING COMPLETED ***\n")
cat("Summary log saved in: outputs/logs/", log_file_name, "\n")
cat("======================================================\n")