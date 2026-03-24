#---- Cleaning ----
rm(list=ls())
graphics.off()

#---- Packages ----
if(!require(here)) install.packages('here'); library(here)
if(!require(readxl)) install.packages('readxl'); library(readxl)
if(!require(urca)) install.packages('urca'); library(urca)
if(!require(tseries)) install.packages('tseries'); library(tseries)
if(!require(lubridate)) install.packages('lubridate'); library(lubridate)
if(!require(zoo)) install.packages("zoo"); library(zoo)
if(!require(seasonal)) install.packages('seasonal'); library(seasonal)
if(!require(midasr)) install.packages('midasr'); library(midasr)
if(!require(ggplot2)) install.packages('ggplot2'); library(ggplot2)

#---- Necessary directories ----
dir.create(here("data/processed"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("graphs/seasonal_adjustment"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("graphs/stationarity"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("graphs/comparison"), showWarnings = FALSE, recursive = TRUE)

#---- Import functions ----

# 1. Data preprocessing
source(here("functions/data_preprocessing/stationarity_analysis.R"))
source(here("functions/data_preprocessing/dickey_fuller_table.R"))
source(here("functions/data_preprocessing/stationarization_scheme.R"))
source(here("functions/data_preprocessing/decumulate.R"))
source(here("functions/data_preprocessing/seasonal_adjustment.R"))
source(here("functions/data_preprocessing/vintage_transformation.R"))
source(here("functions/data_preprocessing/utils.R"))
source(here("functions/baing.R"))
source(here("functions/midas.R"))
source(here("functions/pseudo_real_time.R"))
source(here("functions/plots/plot_nowcast.R"))

# 2. Plots
source(here("functions/plots/plot_series.R"))
source(here("functions/plots/plot_comparison.R"))

#---- Import raw data by countries ----
# We suppose that data per country is in "data/data_country.xlsx"
files <- list.files(here("data/cleaned"), pattern = "\\.xlsx$", full.names = TRUE)
for (f in files) {
  base_name <- tools::file_path_sans_ext(basename(f))
  if (grepl("data", base_name)) {
    name_of_country <- paste0("data_", sub("^data_", "", base_name))
    sheets_of_country <- list(
      Revenues     = read_excel(f, sheet = "Revenues"),
      Expenditures = read_excel(f, sheet = "Expenditures")
    )
    assign(name_of_country, sheets_of_country, envir = .GlobalEnv)
  } else {
    next
  }
}
countries <- list(
  Germany = data_germany,
  Italy   = data_italy,
  France  = data_france,
  Spain   = data_spain
)

#---- Decumulation ----

# Differentiate month to month for cash data
if (file.exists(here("data/processed/countries_decum.rds"))) {
  countries_decum <- readRDS(here("data/processed/countries_decum.rds"))
} else {
  countries_decum <- lapply(countries, decumulate_country,
                            rev_cols = "Cash revenues",
                            exp_cols = "Cash expenditures")
  saveRDS(countries_decum, here("data/processed/countries_decum.rds"))
}

#---- Seasonal adjustment ----
if (file.exists(here("data/processed/countries_sa.rds"))) {
  countries_sa <- readRDS(here("data/processed/countries_sa.rds"))
} else {
  countries_sa <- lapply(countries_decum, seasonally_adjust_country)
  saveRDS(countries_sa, here("data/processed/countries_sa.rds"))
}

#---- ADF tests ----
if (file.exists(here("data/processed/results_adf.rds"))) {
  results_ADF <- readRDS(here("data/processed/results_adf.rds"))
} else {
  results_ADF <- lapply(countries_sa, get_results_ADF_per_country)
  saveRDS(results_ADF, here("data/processed/results_adf.rds"))
}

#---- Stationarization ----
if (file.exists(here("data/processed/countries_statio.rds"))) {
  countries_statio <- readRDS(here("data/processed/countries_statio.rds"))
} else {
  countries_statio <- mapply(stationarize_country,
                             country_data = countries_sa,
                             country_diag = results_ADF,
                             SIMPLIFY = FALSE)
  saveRDS(countries_statio, here("data/processed/countries_statio.rds"))
}

# Plot raw series and stationarized versions
mapply(plot_country_pdf,
       country_data = countries,
       country_stat = countries_statio,
       country_name = names(countries),
       file_prefix = "stationarity/statio")

plot_comparison_pdf(countries, countries_statio, "Total expenditures accrual", "Expenditures")
plot_comparison_pdf(countries, countries_statio, "Total revenues accrual", "Revenues")


# ==============================================================================
# ----------------------------- ESTIMATION (FINAL LOOP) ------------------------
# ==============================================================================

# 1. Create directory for nowcast plots
dir.create(here("graphs/nowcast"), showWarnings = FALSE, recursive = TRUE)

# 2. Define loop parameters
list_countries <- names(countries) # "Germany", "Italy", "France", "Spain"
list_blocks <- c("Revenues", "Expenditures")

# 3. The Main Loop
for (country in list_countries) {
  
  # A. Load publication delays specific to the current country
  df_publi_delay <- read_excel(here("data/publication_delay.xlsx"), sheet = country)
  colnames(df_publi_delay) <- trimws(colnames(df_publi_delay))
  
  for (block in list_blocks) {
    cat("\n======================================================\n")
    cat(">>> Starting Nowcast :", country, "-", block, "\n")
    cat("======================================================\n")
    
    # B. Extract stationarized data (dropping the first 3 months / 1st quarter)
    df_statio <- countries_statio[[country]][[block]][-(1:3), ]
    
    # C. Create the Ragged Edge dataset
    ragged_df <- ragged_edge_dataset(df_statio, df_publi_delay)
    
    # D. Specific cleaning for Expenditures
    if (block == "Expenditures") {
      # Safely drop the text column to avoid PCA crashes
      ragged_df[["Sovereign credit rating revisions"]] <- NULL
    }
    
    # E. Run the model in Pseudo Real-Time
    nowcast_result <- pseudo_real_time(
      df_ragged    = ragged_df,
      start_date   = "2013-01-01",
      method       = "expanding",
      n_window     = 36,
      kmax         = 4,
      lags_factors = 2
    )
    
    # F. Save raw results (.rds)
    file_name_rds <- paste0("nowcast_", tolower(country), "_", tolower(block), ".rds")
    path_rds <- here("data/processed", file_name_rds)
    saveRDS(nowcast_result, path_rds)
    
    # G. Generate plot
    # Pass a custom title for the plot (e.g., "France - Revenues")
    plot_title <- paste(country, "-", block)
    my_plot <- plot_nowcast_vs_realized(
      nowcast_df   = nowcast_result,
      ragged_df    = ragged_df,
      country_name = plot_title
    )
    
    # H. Save plot (.pdf)
    file_name_pdf <- paste0("nowcast_", tolower(country), "_", tolower(block), ".pdf")
    path_pdf <- here("graphs/nowcast", file_name_pdf)
    ggsave(filename = path_pdf, plot = my_plot, width = 12, height = 7, dpi = 300)
    
    cat("[DONE] Saved :", file_name_rds, "and", file_name_pdf, "\n")
  }
}

cat("\n*** ALL NOWCASTS ARE COMPLETED ! ***\n")