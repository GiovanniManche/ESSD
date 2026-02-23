#---- Cleaning ----
rm(list=ls())
graphics.off()
setwd(dir=dirname(rstudioapi::getSourceEditorContext()$path))

#---- Packages ----
if(!require(rstudioapi)) install.packages('rstudioapi'); library(rstudioapi)
if(!require(readxl)) install.packages('readxl'); library(readxl)
if(!require(urca)) install.packages('urca'); library(urca)
if(!require(tseries)) install.packages('tseries'); library(tseries)

#---- Import functions ----
# 1. Data preprocessing
source("functions/data_preprocessing/stationarity_analysis.R")
source("functions/data_preprocessing/dickey_fuller_table.R")
source("functions/data_preprocessing/stationarization_scheme.R")

# 2. Plots
source("functions/plots/plot_series.R")

#---- Import raw data by countries ----
# We suppose that data per country is in "data_country.xlsx"
files <- list.files("data", pattern = "\\.xlsx$", full.names = TRUE)
for (f in files) {
  base_name <- tools::file_path_sans_ext(basename(f))
  name_of_country <- paste0("data_", sub("^data_", "", base_name))
  sheets_of_country <- list(
    Revenues     = read_excel(f, sheet = "Revenues"),
    Expenditures = read_excel(f, sheet = "Expenditures")
  )
  assign(name_of_country, sheets_of_country, envir = .GlobalEnv)
}

results_ADF_germany <- get_results_ADF_per_country(data_germany)
results_ADF_italy <- get_results_ADF_per_country(data_italy)
 
data_germany_statio <- stationarize_country(data_germany, results_ADF_germany)
data_italy_statio <- stationarize_country(data_italy, results_ADF_italy)


plot_country_pdf(data_germany, data_germany_statio, "Germany")
plot_country_pdf(data_italy, data_italy_statio, "Italy")
