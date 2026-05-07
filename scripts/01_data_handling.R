# ==============================================================================
# SCRIPT 01: DATA HANDLING
# ==============================================================================
# This script loads raw data, perform standard seasonality and stationarity
# treatment and save results in a rds. file

cat("\n======================================================\n")
cat(">>> STARTING DATA PREPROCESSING PIPELINE\n")
cat("======================================================\n")


# ---- 1. Import data ----
files <- list.files(here("data/raw"), pattern = "\\.xlsx$", full.names = TRUE)
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


# ---- 2. Sequential treatment ---
# Decumulation (typically cash revenues and expenditures are 
# expressed in year-to-date)
countries_decum <- lapply(countries, decumulate_country)

# Seasonal adjustment
countries_sa <- lapply(countries_decum, seasonally_adjust_country)

# Augmented Dickey Fuller test
results_ADF <- lapply(countries_sa, get_results_ADF_per_country)

# Stationarisation 
countries_statio <- mapply(stationarize_country, 
                           country_data = countries_sa, 
                           country_diag = results_ADF, 
                           SIMPLIFY = FALSE)

# --- 3. Save ---
processed_data <- list(
  raw      = countries,
  sa       = countries_sa,      
  adf      = results_ADF,
  statio   = countries_statio   
)
saveRDS(processed_data, here("data/processed/data_full_treatments.rds"))
message(">>> Raw, seasonally adjusted and stationarized data saved.")