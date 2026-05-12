# ==============================================================================
# GENERAL CONFIGURATION OF THE PROJECT
# ==============================================================================

# ---- 1. Packages ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  here,       
  readxl,     
  dplyr,      
  tidyr,      
  lubridate,  
  ggplot2,    
  patchwork,  
  urca,       
  seasonal,
  patchwork,
  midasr,     
  zoo,
  midasml,
  fixest,
  sandwich,
  lmtest,
  purrr,
  writexl
)




# ---- 2. Create structure ----
dirs <- c(
  here("data/raw"),
  here("data/processed"),
  here("R/nowcasting"),
  here("R/plots"),
  here("R/preprocessing"),
  here("R/MP"),
  here("outputs/figures/nowcast"),
  here("outputs/figures/surprises"),
  here("outputs/figures/index"),
  here("outputs/figures/monetary_policy"),
  here("outputs/tables/nowcast_performances"),
  here("outputs/tables/index"),
  here("outputs/tables/monetary_policy"),
  here("outputs/logs"),
  here("reports/"),
  here("scripts/")
)
invisible(lapply(dirs, dir.create, showWarnings = FALSE, recursive = TRUE))

# ---- 3. Load functions ----
if (dir.exists(here("R"))) {
  func_files <- list.files(here("R"), pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  invisible(lapply(func_files, source))
  message(sprintf(">>> %d functions loaded", length(func_files)))
}

# ---- 4. Global options ----
options(scipen = 999)      # No scientific writing 
theme_set(theme_minimal()) # Default theme for ggplot2

message(">>> Environment configuration succeeded.")
