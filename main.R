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

#---- Import functions ----

# 1. Data preprocessing
source(here("functions/data_preprocessing/stationarity_analysis.R"))
source(here("functions/data_preprocessing/dickey_fuller_table.R"))
source(here("functions/data_preprocessing/stationarization_scheme.R"))
source(here("functions/data_preprocessing/vintage_transformation.R"))
source(here("functions/data_preprocessing/utils.R"))

# 2. Plots
source(here("functions/plots/plot_series.R"))

#---- Import raw data by countries ----
# We suppose that data per country is in "data/data_country.xlsx"
files <- list.files(here("data"), pattern = "\\.xlsx$", full.names = TRUE)
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

#---- Stationarity analysis ----
results_ADF <- lapply(countries, get_results_ADF_per_country)
data_statio <- mapply(stationarize_country,
                      country_data = countries,
                      country_diag = results_ADF,
                      SIMPLIFY = FALSE)

# Plot raw series and stationarized versions
mapply(plot_country_pdf,
       country_data = countries,
       country_stat = data_statio,
       country_name = names(countries))

plot_comparison_pdf(countries, data_statio, "Total expenditure", "Expenditures")
plot_comparison_pdf(countries, data_statio, "Total revenue", "Revenues")

#---- Test for the ragged edge dataset ----
df_publi_delay <- read_excel(here("data/publication_delay.xlsx"), sheet = "Italy")
test_df        <- data_italy[["Revenues"]]
test_df_ragged    <- ragged_edge_dataset(test_df, df_publi_delay)
test_df_rolling   <- available_data(test_df_ragged, 36, test_df_ragged[48, 1], method = "rolling")
test_df_expanding <- available_data(test_df_ragged, 36, test_df_ragged[48, 1], method = "expanding")