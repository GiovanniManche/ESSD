# ==============================================================================
# CONFIGURATION
# ==============================================================================

# This is where the user can modify the different parameters before running the main.
# It helps centralize all these parameters in one clear file.


# ---- 1. Pipeline switches ----
REFRESH_CLEANING <- FALSE # TRUE = script 01 (data preprocessing) is launched 
REFRESH_MODELS   <- FALSE # TRUE = script 02 (nowcasting estimates) is launched.

# ---- 2. Nowcasting scope ----
# Countries to nowcast
list_countries <- c("Germany", "France", "Italy", "Spain")

# Blocks of public deficit to nowcast (no real reason to nowcast one and not the
# other in reality)
list_blocks    <- c("Revenues", "Expenditures")

# Models to use (AR1 and Random Walk are benchmark more than real tools, as they
# don't allow for surprises computation.)
list_models    <- c("factor_midas", "sg_lasso", "ar1", "rw")

# Nowcasting window 
start_nowcast <- "2013-01-01"
window_method <- "expanding"   # or "rolling"

# If expanding window, this parameter is ignored
n_window <- 36

# ---- 3. Factor-MIDAS parameters ----
# Maximum number of latent factors tested/retained in the factor extraction step.
kmax_factor <- 4

# Number of lags of the extracted factors included in the MIDAS regression.
lags_factors_factor <- 2

# ---- 4. sg-LASSO-MIDAS parameters ----
# Number of monthly lags used for each predictor; 
# x_lags = 2 means current month plus two past months.
x_lags_sgl <- 2

# Number of quarterly autoregressive lags of the target included as additional regressors.
y_lags_sgl <- 1

# Degree of the Legendre polynomial basis used to compress each monthly MIDAS lag block.
# degree = 0 gives one transformed coefficient per predictor.
# Refer to the technical appendix of the report for more details.
legendre_degree_sgl <- 0

# Sparse-group LASSO mixing parameter; gamma = 1 is pure LASSO, gamma = 0 is pure group LASSO.
gamma_sgl <- 0.5

# Number of folds used in time-series cross-validation; if NULL, it is chosen automatically from the estimation sample size.
K_sgl <- NULL

# Length of each validation block in tscv.sglfit.
l_sgl <- 2

# If FALSE, quarterly autoregressive lags of the target are included but not penalized.
penalize_y_lags_sgl <- FALSE