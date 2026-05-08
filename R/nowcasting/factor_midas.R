# ==========================================================================
# FACTOR MIDAS
# ==========================================================================
# This file contains all the function associated with the estimation of a 
# Factor MIDAS. We use it to nowcast the expenditures and revenues
# of a given country in our case.
# The idea is to use principal components on a high-dimensional dataset with
# monthly data, yielding monthly factors then used to predict a quarterly variable
# using standard MIDAS polynomial regression.


factor_estimation <- function(X, kmax, fixed = F){
  # =========================================================================
  # DESCRIPTION
  # Function to estimate principal components.
  # The optimal number of factors is given by a Bai-Ng criterion.
  # -------------------------------------------------------------------------
  # INPUTS
  #     X       : matrix containing the standardized series on which we have to run PCAs. 
  #               column (dates in first). 
  #     kmax    : maximal number of factors
  #     fixed   : False if the number of factors must be determined by Bai-Ng. 
  #
  # OUTPUT
  #     factors : estimated PCs (+ number selected by Bai-Ng if relevant)
  #
  # -------------------------------------------------------------------------
  # We compute the factors
  bn_out <- baing(X = X, kmax = kmax, jj = 2)
  
  # Get optimal number of factors via Bai Ng criterion
  if(fixed == "F"){
    nb_factors <- bn_out$ic1
    
    # Fallback to 1 factor if Bai-Ng selects 0
    if (nb_factors == 0) {
      nb_factors <- 1
    }
    
    # Case where we apply this function to only retrieve the first factor
  }else{
    nb_factors <- 1
  }
  
  factors <- bn_out$Fhat[, 1:nb_factors, drop = FALSE]
  if(fixed == F){
    return(c("factors" = factors, "nb_factors" = nb_factors))
  }else{
    return(factors)
  }
}




factor_midas <- function(df_input, kmax = 3, lags_factors = 2){
  # =========================================================================
  # DESCRIPTION
  # Function to estimate a Factor-Midas model
  # Factors are estimated via PCA (excluding the target), 
  # with the optimal number of factors defined by the Bai-Ng criterion. 
  # -------------------------------------------------------------------------
  # INPUTS
  #     df_input      : dataframe containing the series. Target variable should be in second 
  #               column (dates in first). 
  #     kmax          : maximal number of factors
  #     lags_factors  : number of lags to be used in the MIDAS regression for the factors
  #                   ex: if = 2, take current, past month and month before (whole quarter)
  #
  # OUTPUTS
  #     model         : results of the MIDAS estimation
  #     y_target      : aligned quarterly target variable
  #     nb_factors    : number of factors given by Bai-Ng criterion
  #     factors       : nb_factors-first principal components
  #     nowcast       : out-of-sample nowcast for the current quarter
  #
  # -------------------------------------------------------------------------
  
  # Cleaning 
  df <- df_input[, colSums(is.na(df_input)) < nrow(df_input)]
  dates <- as.Date(df[[1]])  
  # Target is supposed to be the second column, Dates in first
  Y_raw <- as.numeric(df[, 2])
  X_raw <- df[, -c(1, 2)]
  
  # Remove NA (CAUTION TO BE MODIFIED LATER VIA EM OR KF, TEMPORARY)
  X_filled <- zoo::na.locf(X_raw, na.rm = FALSE, fromLast = FALSE)
  X_filled <- zoo::na.locf(X_filled, na.rm = FALSE, fromLast = TRUE)
  
  # Scaling
  X_scaled <- scale(X_filled)
  
  res <- factor_estimation(X_scaled, kmax)
  factors <- res["factors"]
  nb_factors <- res["nb_factors"]
  
  # Separate estimation and nowcast periods 
  n_months_total <- nrow(factors)
  
  # Build quarterly target from every 3rd month
  n_q_total <- floor(n_months_total / 3)
  y_all <- Y_raw[seq(3, n_q_total * 3, by = 3)]
  
  # Find last non-NA quarter 
  non_na_idx <- which(!is.na(y_all))
  if (length(non_na_idx) == 0) stop("No non-NA quarterly target values found")
  last_obs_q <- max(non_na_idx)
  
  # Estimation sample: only quarters with known target
  y_est <- y_all[1:last_obs_q]
  y_ts  <- ts(as.numeric(scale(y_est)), frequency = 4)
  
  # Store scaling parameters for the nowcast
  y_mean <- mean(y_est, na.rm = TRUE)
  y_sd   <- sd(y_est, na.rm = TRUE)
  
  # Factor data for estimation (must be exactly last_obs_q * 3 months)
  n_est_months <- last_obs_q * 3
  F_est <- factors[1:n_est_months, , drop = FALSE]
  
  # MIDAS estimation 
  midas_env <- new.env()
  assign("y_ts", y_ts, envir = midas_env)
  
  for (i in 1:nb_factors) {
    assign(paste0("F", i), ts(F_est[, i], frequency = 12), envir = midas_env)
  }
  
  # Dynamic formula (nb of factors can change)
  base_formula <- "y_ts ~ mls(y_ts, 1, 1)"
  fact_terms <- sapply(1:nb_factors, function(i) {
    paste0("mls(F", i, ", 0:", lags_factors, ", 3)")
  })
  full_formula <- paste(base_formula, "+", paste(fact_terms, collapse = " + "))
  final_formula <- as.formula(full_formula)
  environment(final_formula) <- midas_env
  
  fit <- midas_r(final_formula, start = NULL, na.action = na.exclude)
  
  # OOS nowcast 
  # Collect factor data beyond the estimation period (= nowcast quarter)
  n_new_months <- n_months_total - n_est_months
  
  if (n_new_months > 0) {
    F_new <- factors[(n_est_months + 1):n_months_total, , drop = FALSE]
  } else {
    # At quarter boundary: forward-fill last factor values
    F_new <- factors[n_months_total, , drop = FALSE]
  }
  
  # Pad to exactly 3 months if we have < 3 months of new factor data
  if (nrow(F_new) < 3) {
    last_row <- F_new[nrow(F_new), , drop = FALSE]
    pad <- last_row[rep(1, 3 - nrow(F_new)), , drop = FALSE]
    F_new <- rbind(F_new, pad)
  }
  # Keep only the first 3 months (one quarter ahead)
  if (nrow(F_new) > 3) {
    F_new <- F_new[1:3, , drop = FALSE]
  }
  
  # Manual out-of-sample forecast: x_new %*% coef(fit)
  # Coefficient order: (Intercept), y_ts_lag1, F1.lag0, F1.lag1, ..., F2.lag0, ...
  coefs <- coef(fit)
  
  # AR(1) regressor: last known standardized y
  ar_val <- tail(as.numeric(y_ts), 1)
  
  # Build regressor vector
  x_new <- c(1, ar_val)  # intercept + AR(1)
  for (i in 1:nb_factors) {
    # F_new[,i] is [month1, month2, month3] in chronological order
    # mls ordering: lag0 = month3 (most recent), lag1 = month2, lag2 = month1
    fi_lags <- rev(F_new[, i])
    x_new <- c(x_new, fi_lags[1:(lags_factors + 1)])
  }
  
  nowcast_val_scaled <- as.numeric(x_new %*% coefs)
  nowcast_val <- nowcast_val_scaled * y_sd + y_mean
  
  return(list(
    model      = fit,
    y_target   = y_ts,
    nb_factors = nb_factors,
    factors    = factors,
    loadings = bn_out$Lambda[, 1:nb_factors, drop = FALSE],
    nowcast_scaled    = nowcast_val_scaled,
    nowcast = nowcast_val,
    y_mean = y_mean,
    y_sd = y_sd,
    date_new = dates[n_months_total],
    last_obs_y = y_all[last_obs_q]  
  ))
}