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

#---- Test for the ragged edge dataset ----
df_publi_delay <- read_excel(here("data/publication_delay.xlsx"), sheet = "Italy")
test_df        <- data_italy[["Revenues"]]
test_df_ragged    <- ragged_edge_dataset(test_df, df_publi_delay)
test_df_rolling   <- available_data(test_df_ragged, 36, test_df_ragged[48, 1], method = "rolling")
test_df_expanding <- available_data(test_df_ragged, 36, test_df_ragged[48, 1], method = "expanding")
