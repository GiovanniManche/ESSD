# ==============================================================================
# MAIN CODE
# ==============================================================================
# Entry point of the pipeline. It coordinates 
# Data Preparation (01), Estimation (02), and Visualization (03).

# ---- 0. Load environment and functions ----
source("_setup.R")

# Load parameters used throughout the entire run.
# If you need to change any parameter, please do so in the "configuration.R" file
# prior to running the code.
source("configuration.R")


# ---- 1. Data preprocessing  ----
# The script 01 is run if processed data has not been saved or
# if the user wants to refresh (REFRESH_CLEANING = TRUE)
path_master_data <- here("data/processed/data_full_treatments.rds")

if (!file.exists(path_master_data) | REFRESH_CLEANING) {
  cat("\n>>> STEP 1: Running data cleaning and stationarization...\n")
  source("scripts/01_data_handling.R")
} else {
  cat("\n>>> STEP 1: Processed data found.\n")
}

# ---- 2. Estimation step (Nowcasting loop) ----
# The script 02 is run if one needs to refresh the results,
# for instance if there has been changes in the model's parameters.
# Can be time-consuming if running for all countries.
# See more in the 02 script.

path_nowcast_results <- here("data/processed/nowcast_all_results.rds")

if (!file.exists(path_nowcast_results) | REFRESH_MODELS) {
  cat("\n>>> STEP 2: Running nowcast estimations...\n")
  source("scripts/02_nowcasting_estimation.R")
} else {
  cat("\n>>> STEP 2: Using existing .rds results for models.\n")
  nowcast_all_results <- readRDS(path_nowcast_results)
}

# ---- 3. Nowcast visualization step  ----
# We always run this to ensure plots are up to date with the latest results
cat("\n>>> STEP 3: Generating Final Plots & Grids...\n")
source("scripts/03_visualisation.R")

# ---- 4. Nowcast performance metrics
source("scripts/04_forecast_metrics.R")

# ---- 5. Computation of the surprise index ----
source("scripts/05_surprise_index.R")

# ---- Final Summary ----
cat("\n======================================================")
cat("\nPIPELINE EXECUTION FINISHED SUCCESSFULLY")
cat("\n- Data: ", path_master_data)
cat("\n- Plots: ", here("outputs/figures/nowcast/"))
cat("\n======================================================\n")