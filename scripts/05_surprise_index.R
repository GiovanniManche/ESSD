# ==============================================================================
# SCRIPT 05: NOWCAST SURPRISE INDEX
# ==============================================================================

# ---- 0. Environment Setup ----
if (!exists("pseudo_real_time")) source("_setup.R")

data_processed_all <- readRDS(here("data/processed/data_full_treatments.rds"))

cat("\n======================================================\n")
cat(">>> STARTING SURPRISE INDEX PIPELINE\n")
cat("======================================================\n")

surprise_models <- intersect(list_models, c("factor_midas", "sg_lasso"))

if (length(surprise_models) == 0) {
  stop("No valid surprise models found. Expected factor_midas and/or sg_lasso in list_models.")
}


# ---- 1. Surprises computation loop ----
all_surprises <- list()

for (model_name in surprise_models) {
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
      nowcast_ok <- nowcast_df[
        nowcast_df$status == "OK" & !is.na(nowcast_df$nowcast_value),
      ]
      
      if (nrow(nowcast_ok) == 0) {
        warning(sprintf("[SKIP] No valid nowcasts for %s - %s - %s",
                        model_name, country, block))
        next
      }
      
      nowcast_ok$publication_date <- as.Date(nowcast_ok$publication_date)
      
      df_publi_delay <- read_excel(
        here("data/publication_delay.xlsx"),
        sheet = country
      )
      colnames(df_publi_delay) <- trimws(colnames(df_publi_delay))
      
      df_statio <- data_processed_all$statio[[country]][[block]][-(1:3), ]
      
      if (block == "Expenditures" &&
          "Sovereign credit rating revisions" %in% colnames(df_statio)) {
        df_statio[["Sovereign credit rating revisions"]] <- NULL
      }
      
      actual_targets <- df_statio[!is.na(df_statio[[2]]), c(1, 2)]
      colnames(actual_targets) <- c("target_date", "actual")
      actual_targets$target_date <- as.Date(actual_targets$target_date)
      actual_targets$actual      <- as.numeric(actual_targets$actual)
      
      target_var  <- colnames(df_statio)[2]
      month_delay <- as.numeric(df_publi_delay[2, target_var])
      day_delay   <- as.numeric(df_publi_delay[3, target_var])
      
      target_pubs <- as.Date(date_modifyer(
        actual_targets$target_date,
        month = month_delay,
        days  = day_delay
      ))
      
      if (length(target_pubs) != nrow(actual_targets)) {
        warning(sprintf("[SKIP] Target length mismatch for %s - %s", country, block))
        next
      }
      
      actual_targets$target_pub_date <- target_pubs
      
      nowcast_ok$target_date     <- as.Date(NA)
      nowcast_ok$target_pub_date <- as.Date(NA)
      
      for (i in seq_len(nrow(nowcast_ok))) {
        pub_date  <- nowcast_ok$publication_date[i]
        past_pubs <- which(actual_targets$target_pub_date <= pub_date)
        
        if (length(past_pubs) > 0) {
          last_obs_idx    <- max(past_pubs)
          next_target_idx <- last_obs_idx + 1
          
          if (next_target_idx <= nrow(actual_targets)) {
            nowcast_ok$target_date[i]     <- actual_targets$target_date[next_target_idx]
            nowcast_ok$target_pub_date[i] <- actual_targets$target_pub_date[next_target_idx]
          }
        }
      }
      
      mapped <- nowcast_ok[!is.na(nowcast_ok$target_date), ]
      
      if (nrow(mapped) < 2) {
        warning(sprintf("[SKIP] Insufficient mapped nowcasts for %s - %s - %s",
                        model_name, country, block))
        next
      }
      
      mapped <- mapped %>%
        arrange(target_date, publication_date) %>%
        group_by(target_date) %>%
        mutate(
          surprise_raw = nowcast_value - lag(nowcast_value),
          surprise     = ifelse(is.na(surprise_raw), 0, surprise_raw),
          year_month   = format(publication_date, "%Y-%m")
        ) %>%
        ungroup() %>%
        mutate(
          model   = model_name,
          country = country,
          block   = block
        )
      
      all_surprises[[length(all_surprises) + 1]] <- mapped
    }
  }
}

if (length(all_surprises) == 0) {
  stop("No valid nowcasts found to compute surprises.")
}

df_all_surprises <- bind_rows(all_surprises)


# ---- 2. Export raw surprises ----
write.csv(
  df_all_surprises,
  here("outputs/tables/index", "all_nowcast_surprises.csv"),
  row.names = FALSE
)


# ---- 2b. Raw surprise plots ----
# One figure per (block x model), with one panel per country.
# Each panel shows all intra-monthly nowcast revisions over time.
# Total block = Revenues - Expenditures, computed before monthly aggregation.

df_surprises_plot <- df_all_surprises %>%
  select(publication_date, target_date, year_month, model, country, block, surprise) %>%
  pivot_wider(
    names_from  = block,
    values_from = surprise,
    values_fill = list(surprise = 0)
  )

if (!"Revenues"     %in% colnames(df_surprises_plot)) df_surprises_plot$Revenues     <- 0
if (!"Expenditures" %in% colnames(df_surprises_plot)) df_surprises_plot$Expenditures <- 0

df_surprises_plot <- df_surprises_plot %>%
  mutate(Total = Revenues - Expenditures) %>%
  pivot_longer(
    cols      = c(Revenues, Expenditures, Total),
    names_to  = "block",
    values_to = "surprise"
  ) %>%
  mutate(publication_date = as.Date(publication_date))

raw_plot_blocks <- unique(c(list_blocks, "Total"))

for (b in raw_plot_blocks) {
  for (mod in surprise_models) {
    
    df_plot_raw <- df_surprises_plot %>%
      filter(block == b, model == mod)
    
    if (nrow(df_plot_raw) == 0) next
    
    p_raw <- ggplot(df_plot_raw, aes(x = publication_date, y = surprise)) +
      geom_col(
        data = df_plot_raw %>% filter(surprise >= 0),
        fill = "#2ecc71", alpha = 0.8, width = 20
      ) +
      geom_col(
        data = df_plot_raw %>% filter(surprise < 0),
        fill = "#e74c3c", alpha = 0.8, width = 20
      ) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.4) +
      facet_wrap(~ country, ncol = 2, scales = "free_y") +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      theme_minimal(base_size = 11) +
      theme(
        legend.position    = "none",
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title         = element_text(face = "bold", size = 13),
        plot.subtitle      = element_text(size = 10, color = "grey30"),
        strip.text         = element_text(face = "bold"),
        axis.text.x        = element_text(angle = 45, hjust = 1)
      ) +
      labs(
        title    = paste0("Raw Nowcast Surprises — ", b),
        subtitle = paste0(
          "Intra-monthly nowcast revisions (surprise = nowcast_t − nowcast_{t−1})\n",
          "Model: ", mod, " | Green = positive surprise, Red = negative surprise"
        ),
        x       = NULL,
        y       = "Nowcast revision",
        caption = "Note: each bar is a single nowcast update."
      )
    
    ggsave(
      here("outputs/figures/surprises",
           paste0("raw_surprises_", tolower(b), "_", mod, ".pdf")),
      p_raw, width = 12, height = 8
    )
    cat(sprintf(">>> [OK] Raw surprise plot saved: %s - %s\n", b, mod))
  }
}


# ---- 3. Monthly country-level surprise index ----

# Aggregate intra-month nowcast revisions into a single monthly surprise per country
country_monthly_index <- df_all_surprises %>%
  group_by(country, model, block, year_month) %>%
  summarize(
    mensual_surprise = sum(surprise, na.rm = TRUE),
    n_updates        = n(),
    .groups          = "drop"
  )

# Construct "Total" block = Revenues - Expenditures
country_total_index <- country_monthly_index %>%
  select(country, model, block, year_month, mensual_surprise, n_updates) %>%
  pivot_wider(
    names_from  = block,
    values_from = c(mensual_surprise, n_updates),
    values_fill = list(mensual_surprise = 0, n_updates = 0)
  )

if (!"mensual_surprise_Revenues"     %in% colnames(country_total_index)) country_total_index$mensual_surprise_Revenues     <- 0
if (!"mensual_surprise_Expenditures" %in% colnames(country_total_index)) country_total_index$mensual_surprise_Expenditures <- 0
if (!"n_updates_Revenues"            %in% colnames(country_total_index)) country_total_index$n_updates_Revenues            <- 0
if (!"n_updates_Expenditures"        %in% colnames(country_total_index)) country_total_index$n_updates_Expenditures        <- 0

country_total_index <- country_total_index %>%
  mutate(
    block            = "Total",
    mensual_surprise = mensual_surprise_Revenues - mensual_surprise_Expenditures,
    n_updates        = n_updates_Revenues + n_updates_Expenditures
  ) %>%
  select(
    country, model, block, year_month, mensual_surprise, n_updates,
    mensual_surprise_Revenues, mensual_surprise_Expenditures,
    n_updates_Revenues, n_updates_Expenditures
  )

country_monthly_index_extended <- bind_rows(
  country_monthly_index,
  country_total_index %>% select(country, model, block, year_month, mensual_surprise, n_updates)
)


# ---- 3b. GDP weights (quarterly) ----
gdp_raw <- read.csv(
  here("data/gdp_series.csv"),
  sep              = ";",
  stringsAsFactors = FALSE,
  na.strings       = c("", "NA")
)

gdp_long <- gdp_raw %>%
  rename(date = DATE, period = TIME.PERIOD) %>%
  pivot_longer(
    cols      = c(Germany, Spain, France, Italy),
    names_to  = "country",
    values_to = "gdp"
  ) %>%
  mutate(
    gdp     = as.numeric(gdp),
    year    = as.integer(substr(period, 1, 4)),
    quarter = as.integer(substr(period, 6, 6))
  ) %>%
  filter(!is.na(gdp)) %>%
  group_by(year, quarter) %>%
  mutate(weight = gdp / sum(gdp, na.rm = TRUE)) %>%
  ungroup() %>%
  select(country, year, quarter, weight)


# ---- 4. Normalisation and euro area indices ----

# We construct two distinct euro area indices:
#
# (1) Fiscal Surprise Climate Index (climate_index):
#     GDP-weighted average of country-level z-scores, rescaled to mean 100 / sd 10.
#     Above 100: fiscal news better than historical average across the euro area.
#     Below 100: fiscal news worse than historical average.
#
# (2) Fiscal Fragmentation Index (fragmentation_index):
#     Log of GDP-weighted cross-sectional standard deviation of country z-scores,
#     rescaled to mean 100 / sd 10.
#     Above 100: dispersion higher than historical average (countries diverging).
#     Below 100: dispersion lower than historical average (countries converging).

# Helper: year_month string ("2015-03") -> quarter integer (1, 2, 3, 4)
month_to_quarter <- function(ym) {
  m <- as.integer(substr(ym, 6, 7))
  ceiling(m / 3)
}

# Step 1: z-score each country's surprise series over the full sample
country_monthly_index_extended <- country_monthly_index_extended %>%
  group_by(country, model, block) %>%
  mutate(
    surprise_mean         = mean(mensual_surprise, na.rm = TRUE),
    surprise_sd           = sd(mensual_surprise, na.rm = TRUE),
    surprise_zscore       = ifelse(surprise_sd > 0,
                                   (mensual_surprise - surprise_mean) / surprise_sd, 0),
    climate_index_country = 100 + 10 * surprise_zscore
  ) %>%
  ungroup()

# Step 2: join GDP weights and compute euro area aggregates
# cross_mean_w is pre-computed via mutate before summarize to avoid
# ambiguous evaluation of weighted.mean() inside sum() within summarize()
euro_index_raw <- country_monthly_index_extended %>%
  mutate(
    year    = as.integer(substr(year_month, 1, 4)),
    quarter = month_to_quarter(year_month)
  ) %>%
  left_join(gdp_long, by = c("country", "year", "quarter")) %>%
  group_by(model, block, year_month) %>%
  mutate(cross_mean_w = weighted.mean(surprise_zscore, w = weight, na.rm = TRUE)) %>%
  summarize(
    n_countries          = n_distinct(country),
    cross_sec_sd         = ifelse(n_distinct(country) > 1,
                                  sd(surprise_zscore, na.rm = TRUE), NA_real_),
    cross_sec_sd_w       = ifelse(n_distinct(country) > 1,
                                  sqrt(sum(weight * (surprise_zscore - cross_mean_w)^2,
                                           na.rm = TRUE)),
                                  NA_real_),
    mean_surprise_zscore = weighted.mean(surprise_zscore, w = weight, na.rm = TRUE),
    .groups              = "drop"
  )

# Step 3: rescale to indices — done in a separate explicit step per (model x block)
# to avoid ifelse() swallowing sd()/mean() evaluated on a single-row group
euro_index_rescaling <- euro_index_raw %>%
  group_by(model, block) %>%
  summarize(
    climate_mean   = mean(mean_surprise_zscore,    na.rm = TRUE),
    climate_sd     = sd(mean_surprise_zscore,      na.rm = TRUE),
    log_frag_mean  = mean(log(cross_sec_sd_w),     na.rm = TRUE),
    log_frag_sd    = sd(log(cross_sec_sd_w),       na.rm = TRUE),
    .groups        = "drop"
  )

euro_index <- euro_index_raw %>%
  left_join(euro_index_rescaling, by = c("model", "block")) %>%
  mutate(
    # Climate index: GDP-weighted mean of z-scores, rescaled to mean 100 / sd 10
    climate_index       = ifelse(
      climate_sd > 0,
      100 + 10 * (mean_surprise_zscore - climate_mean) / climate_sd,
      100
    ),
    # Fragmentation index: log(weighted cross-sec sd), rescaled to mean 100 / sd 10
    fragmentation_index = ifelse(
      log_frag_sd > 0,
      100 + 10 * (log(cross_sec_sd_w) - log_frag_mean) / log_frag_sd,
      100
    )
  ) %>%
  select(-climate_mean, -climate_sd, -log_frag_mean, -log_frag_sd) %>%
  arrange(model, block, year_month)


# ---- 5. Save CSVs ----

write.csv(
  country_monthly_index_extended %>%
    select(country, model, block, year_month,
           mensual_surprise, surprise_zscore, climate_index_country),
  here("outputs/tables/index", "country_monthly_surprise_index.csv"),
  row.names = FALSE
)

write.csv(
  euro_index %>%
    select(model, block, year_month, cross_sec_sd, cross_sec_sd_w,
           climate_index, fragmentation_index),
  here("outputs/tables/index", "euro_area_indices.csv"),
  row.names = FALSE
)

cat(">>> [OK] Indices saved.\n")


# ---- 6. Visualizations: euro area and country-level indices ----
cat(">>> Generating visualizations...\n")

dir.create(here("outputs/figures/index"),     showWarnings = FALSE, recursive = TRUE)
dir.create(here("outputs/figures/surprises"), showWarnings = FALSE, recursive = TRUE)

plot_blocks <- unique(c(list_blocks, "Total"))

my_colors <- c(
  "factor_midas" = "#e74c3c",
  "sg_lasso"     = "#3498db"
)

for (b in plot_blocks) {
  
  df_plot <- euro_index %>%
    filter(block == b) %>%
    mutate(date = as.Date(paste0(year_month, "-01")))
  
  if (nrow(df_plot) == 0) next
  
  if (b == "Total") {
    title_climate       <- "Euro Area Fiscal Surprise Index"
    title_fragmentation <- "Euro Area Fiscal Fragmentation Index"
    title_dispersion    <- "Euro Area Cross-Sectional Fiscal Surprise Dispersion"
    subtitle_climate       <- "GDP-weighted average of country z-scores, rescaled to mean 100 / sd 10"
    subtitle_fragmentation <- "GDP-weighted cross-sec. sd of country z-scores (log-rescaled to mean 100 / sd 10)"
    subtitle_dispersion    <- "Raw GDP-weighted cross-sectional standard deviation of country z-scores"
  } else {
    title_climate       <- paste("Euro Area Fiscal Surprise Index —", b)
    title_fragmentation <- paste("Euro Area Fiscal Fragmentation Index —", b)
    title_dispersion    <- paste("Euro Area Cross-Sectional Fiscal Surprise Dispersion —", b)
    subtitle_climate       <- paste("GDP-weighted average of", tolower(b), "surprise z-scores, rescaled to mean 100 / sd 10")
    subtitle_fragmentation <- paste("GDP-weighted cross-sec. sd of", tolower(b), "surprise z-scores (log-rescaled to mean 100 / sd 10)")
    subtitle_dispersion    <- paste("Raw GDP-weighted cross-sectional standard deviation of", tolower(b), "surprise z-scores")
  }
  
  p1 <- ggplot(df_plot, aes(x = date, y = climate_index, color = model)) +
    geom_line(linewidth = 0.8, na.rm = TRUE) +
    geom_hline(yintercept = 100, linetype = "dashed", color = "grey50") +
    scale_color_manual(values = my_colors) +
    theme_minimal() +
    labs(title = title_climate, subtitle = subtitle_climate,
         x = "Date", y = "Index", color = "Model") +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
  
  p2 <- ggplot(df_plot, aes(x = date, y = fragmentation_index, color = model)) +
    geom_line(linewidth = 0.8, na.rm = TRUE) +
    geom_hline(yintercept = 100, linetype = "dashed", color = "grey50") +
    scale_color_manual(values = my_colors) +
    theme_minimal() +
    labs(title = title_fragmentation, subtitle = subtitle_fragmentation,
         x = "Date", y = "Index", color = "Model") +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
  
  p3 <- ggplot(df_plot, aes(x = date, y = cross_sec_sd_w, color = model)) +
    geom_line(linewidth = 0.8, na.rm = TRUE) +
    scale_color_manual(values = my_colors) +
    theme_minimal() +
    labs(title = title_dispersion, subtitle = subtitle_dispersion,
         x = "Date", y = "Cross-sectional standard deviation (GDP-weighted)", color = "Model") +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
  
  df_country_plot <- country_monthly_index_extended %>%
    filter(block == b) %>%
    mutate(date = as.Date(paste0(trimws(year_month), "-01"), format = "%Y-%m-%d"))
  
  p4 <- ggplot(df_country_plot, aes(x = date, y = climate_index_country, color = country)) +
    geom_line(linewidth = 0.8) +
    geom_hline(yintercept = 100, linetype = "dashed", color = "grey50") +
    facet_wrap(~ model, scales = "free_y") +
    theme_minimal() +
    labs(
      title    = paste("Monthly Fiscal Surprise Index by Country —", b),
      subtitle = "Country-level z-score rescaled to mean 100 / sd 10",
      x = "Date", y = "Index", color = "Country"
    ) +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
  
  ggsave(here("outputs/figures/index", paste0("euro_climate_index_",        tolower(b), ".pdf")), p1, width = 10, height = 6)
  ggsave(here("outputs/figures/index", paste0("euro_fragmentation_index_",  tolower(b), ".pdf")), p2, width = 10, height = 6)
  ggsave(here("outputs/figures/index", paste0("euro_cross_sec_dispersion_", tolower(b), ".pdf")), p3, width = 10, height = 6)
  ggsave(here("outputs/figures/index", paste0("country_climate_index_",     tolower(b), ".pdf")), p4, width = 12, height = 7)
}

cat(">>> [OK] Index visualizations saved.\n")


# ---- 7. Country contributions to the Climate Index (INSEE-style) ----
#
# Exact additive decomposition:
#   climate_index_t - 100 = sum_c contrib_index_{c,t}
#
# where:
#   contrib_index_{c,t} = 10 * w_{c,t} * z_{c,t} / climate_sd
#                         - 10 * climate_mean / climate_sd / C
#
# The first term scales each country's weighted z-score to index units.
# The second term distributes the centering constant equally across countries.

country_colors <- c(
  "France"  = "#e74c3c",
  "Germany" = "#2ecc71",
  "Italy"   = "#3498db",
  "Spain"   = "#f39c12"
)

# Retrieve per-(model x block) rescaling parameters used to build climate_index
rescaling_params <- euro_index %>%
  group_by(model, block) %>%
  summarize(
    climate_mean = mean(mean_surprise_zscore, na.rm = TRUE),
    climate_sd   = sd(mean_surprise_zscore,   na.rm = TRUE),
    .groups      = "drop"
  )

contributions_climate <- country_monthly_index_extended %>%
  mutate(
    year    = as.integer(substr(year_month, 1, 4)),
    quarter = month_to_quarter(year_month)
  ) %>%
  left_join(gdp_long,         by = c("country", "year", "quarter")) %>%
  left_join(rescaling_params, by = c("model", "block")) %>%
  filter(!is.na(weight)) %>%
  group_by(model, block, year_month) %>%
  mutate(n_countries = n_distinct(country)) %>%
  ungroup() %>%
  mutate(
    contrib_zscore = weight * surprise_zscore,
    contrib_index  = ifelse(
      climate_sd > 0,
      10 * contrib_zscore / climate_sd - (10 * climate_mean / climate_sd) / n_countries,
      0
    )
  ) %>%
  select(country, model, block, year_month, weight,
         surprise_zscore, contrib_zscore, contrib_index)

# Sanity check: contributions must sum exactly to climate_index - 100
check_climate <- contributions_climate %>%
  group_by(model, block, year_month) %>%
  summarize(sum_contrib = sum(contrib_index, na.rm = TRUE), .groups = "drop") %>%
  left_join(euro_index %>% select(model, block, year_month, climate_index),
            by = c("model", "block", "year_month")) %>%
  mutate(residual = sum_contrib - (climate_index - 100))

cat(sprintf(">>> [Climate] Max decomposition residual: %.2e\n",
            max(abs(check_climate$residual), na.rm = TRUE)))

write.csv(
  contributions_climate,
  here("outputs/tables/index", "country_contributions_climate_index.csv"),
  row.names = FALSE
)

for (b in plot_blocks) {
  for (mod in surprise_models) {
    
    df_contrib <- contributions_climate %>%
      filter(block == b, model == mod) %>%
      mutate(date = as.Date(paste0(year_month, "-01")))
    
    df_line <- euro_index %>%
      filter(block == b, model == mod) %>%
      mutate(date = as.Date(paste0(year_month, "-01")))
    
    if (nrow(df_contrib) == 0 || nrow(df_line) == 0) next
    
    y_min <- min(
      min(df_contrib$contrib_index, na.rm = TRUE),
      min(df_line$climate_index - 100, na.rm = TRUE)
    ) * 1.15
    
    y_max <- max(
      df_contrib %>%
        group_by(date) %>%
        summarize(pos_sum = sum(pmax(contrib_index, 0), na.rm = TRUE)) %>%
        pull(pos_sum),
      max(df_line$climate_index - 100, na.rm = TRUE)
    ) * 1.15
    
    p_climate <- ggplot() +
      geom_col(
        data = df_contrib %>% mutate(contrib_pos = pmax(contrib_index, 0)),
        aes(x = date, y = contrib_pos, fill = country),
        position = "stack", width = 25
      ) +
      geom_col(
        data = df_contrib %>% mutate(contrib_neg = pmin(contrib_index, 0)),
        aes(x = date, y = contrib_neg, fill = country),
        position = "stack", width = 25
      ) +
      geom_line(
        data = df_line,
        aes(x = date, y = climate_index - 100),
        color = "black", linewidth = 0.9
      ) +
      geom_point(
        data = df_line %>% filter(date == max(date)),
        aes(x = date, y = climate_index - 100),
        color = "black", size = 2
      ) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
      scale_fill_manual(values = country_colors) +
      scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
      scale_y_continuous(limits = c(y_min, y_max),
                         labels = function(x) sprintf("%+.1f", x)) +
      theme_minimal(base_size = 11) +
      theme(
        legend.position    = "bottom",
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title         = element_text(face = "bold", size = 13),
        plot.subtitle      = element_text(size = 10, color = "grey30"),
        axis.title         = element_text(size = 10)
      ) +
      labs(
        title    = paste0("Euro Area Fiscal Surprise Index — ", b),
        subtitle = paste0(
          "Country contributions (bars) and aggregate index deviation from 100 (line)\n",
          "Model: ", mod, " | GDP-weighted z-scores"
        ),
        x       = NULL,
        y       = "Contribution to index (index pts, mean = 0)",
        fill    = "Country",
        caption = "Note: bars sum exactly to climate_index − 100. Positive = above-average fiscal surprise."
      )
    
    ggsave(
      here("outputs/figures/index",
           paste0("contributions_climate_", tolower(b), "_", mod, ".pdf")),
      p_climate, width = 12, height = 6.5
    )
    cat(sprintf(">>> [OK] Climate contribution plot: %s - %s\n", b, mod))
  }
}


# ---- 8. Country contributions to the Fragmentation Index ----
#
# Exact additive decomposition of the GDP-weighted cross-sectional variance:
#   sigma^2_{w,t} = sum_c contrib_var_{c,t}
#
# where:
#   contrib_var_{c,t} = w_{c,t} * (z_{c,t} - z_bar_{w,t})^2  >= 0
#
# All contributions are non-negative by construction (squared deviations).
# The black line shows sigma^2_{w,t} and coincides with the top of the stacked bars.
# Note: fragmentation_index is based on log(sigma_{w,t}), which is not additively
# decomposable. This plot therefore shows the underlying weighted variance.

contributions_frag <- country_monthly_index_extended %>%
  mutate(
    year    = as.integer(substr(year_month, 1, 4)),
    quarter = month_to_quarter(year_month)
  ) %>%
  left_join(gdp_long, by = c("country", "year", "quarter")) %>%
  filter(!is.na(weight)) %>%
  group_by(model, block, year_month) %>%
  mutate(
    cross_mean_w = weighted.mean(surprise_zscore, w = weight, na.rm = TRUE),
    contrib_var  = weight * (surprise_zscore - cross_mean_w)^2
  ) %>%
  ungroup() %>%
  mutate(date = as.Date(paste0(year_month, "-01"))) %>%
  select(country, model, block, year_month, date,
         weight, surprise_zscore, cross_mean_w, contrib_var)

# Sanity check: contributions must sum exactly to cross_sec_sd_w^2
check_frag <- contributions_frag %>%
  group_by(model, block, year_month) %>%
  summarize(sum_contrib = sum(contrib_var, na.rm = TRUE), .groups = "drop") %>%
  left_join(euro_index %>% select(model, block, year_month, cross_sec_sd_w),
            by = c("model", "block", "year_month")) %>%
  mutate(residual = sum_contrib - cross_sec_sd_w^2)

cat(sprintf(">>> [Fragmentation] Max variance residual: %.2e\n",
            max(abs(check_frag$residual), na.rm = TRUE)))

write.csv(
  contributions_frag %>% select(-date),
  here("outputs/tables/index", "country_contributions_fragmentation_index.csv"),
  row.names = FALSE
)

for (b in plot_blocks) {
  for (mod in surprise_models) {
    
    df_contrib <- contributions_frag %>%
      filter(block == b, model == mod)
    
    df_line <- euro_index %>%
      filter(block == b, model == mod) %>%
      mutate(date = as.Date(paste0(year_month, "-01")))
    
    if (nrow(df_contrib) == 0 || nrow(df_line) == 0) next
    
    y_max <- max(
      df_contrib %>%
        group_by(date) %>%
        summarize(s = sum(contrib_var, na.rm = TRUE)) %>%
        pull(s),
      na.rm = TRUE
    ) * 1.15
    
    p_frag <- ggplot() +
      geom_col(
        data = df_contrib,
        aes(x = date, y = contrib_var, fill = country),
        position = "stack", width = 25
      ) +
      geom_line(
        data = df_line,
        aes(x = date, y = cross_sec_sd_w^2),
        color = "black", linewidth = 0.9
      ) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
      scale_fill_manual(values = country_colors) +
      scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
      scale_y_continuous(labels = function(x) sprintf("%.2f", x),
                         limits = c(0, y_max)) +
      theme_minimal(base_size = 11) +
      theme(
        legend.position    = "bottom",
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title         = element_text(face = "bold", size = 13),
        plot.subtitle      = element_text(size = 10, color = "grey30")
      ) +
      labs(
        title    = paste0("Euro Area Fiscal Fragmentation — ", b),
        subtitle = paste0(
          "Country contributions to GDP-weighted cross-sectional variance\n",
          "Line = total weighted variance σ²_{w,t}  |  Model: ", mod
        ),
        x       = NULL,
        y       = "Contribution to weighted cross-sectional variance (z-score²)",
        fill    = "Country",
        caption = "Note: bars sum exactly to σ²_{w,t} = Σ_c w_c(z_c − z̄_w)²."
      )
    
    ggsave(
      here("outputs/figures/index",
           paste0("contributions_fragmentation_", tolower(b), "_", mod, ".pdf")),
      p_frag, width = 12, height = 6.5
    )
    cat(sprintf(">>> [OK] Fragmentation contribution plot: %s - %s\n", b, mod))
  }
}

cat(">>> [OK] All visualizations saved.\n")
cat("======================================================\n")
cat("*** SURPRISE INDEX PIPELINE COMPLETED ***\n")
cat("======================================================\n")