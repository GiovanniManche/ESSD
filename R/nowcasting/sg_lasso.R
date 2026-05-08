# ==========================================================================
# SG-LASSO MIDAS
# ==========================================================================
# This file contains all the function associated with the estimation of a 
# Sparse Group Lasso MIDAS. We use it to nowcast the expenditures and revenues
# of a given country in our case.
# The idea is to use structured ML regressions on a high-dimensional dataset with
# mixed-frequencies, reduces the dimensionality of the problem and perform a nowcast.
# See the technical appendix of the report or the following papers for more details:
#   - Babii et al., Machine Learning Time Series Regressions With an 
#                   Application to Nowcasting (2021)  for the model details
#   - The "midasml" package details (created by the same authors and used in the functions)



sg_lasso_midas <- function(df_input,
                           x_lags = 2,
                           y_lags = 1,
                           legendre_degree = 0,
                           gamma = 0.5,
                           K = NULL,
                           l = 2,
                           penalize_y_lags = FALSE) {
  # =========================================================================
  # DESCRIPTION
  # Function to estimate a Sparse-Group LASSO MIDAS model.
  # The function first builds direct MIDAS lag regressors, then transforms
  # each high-frequency lag block using Legendre polynomials. The resulting
  # regressors are estimated with sparse-group LASSO using time-series
  # cross-validation.
  # -------------------------------------------------------------------------
  # INPUTS
  #     df_input: dataframe containing the series. Dates should be in the first
  #               column, target variable in the second column, and predictors
  #               in the remaining columns.
  #     x_lags: number of monthly lags used for each predictor.
  #             ex: if = 2, take current month, previous month and month before.
  #     y_lags: number of quarterly autoregressive lags of the target.
  #     legendre_degree: degree of the Legendre polynomial basis used to compress
  #                      each MIDAS lag block.
  #     gamma: sparse-group LASSO mixing parameter.
  #            gamma = 1 corresponds to pure LASSO, gamma = 0 to group LASSO.
  #     K: number of folds for time-series cross-validation. If NULL, chosen
  #        automatically from the estimation sample size.
  #     l: length of the validation block in tscv.sglfit.
  #     penalize_y_lags: logical. If FALSE, autoregressive y lags are not penalized.
  #
  # OUTPUTS
  #     nowcast: out-of-sample nowcast for the current quarter
  #     lambda: selected regularization parameter
  #     gamma: sparse-group LASSO mixing parameter
  #     cv_mse: minimum cross-validation error
  #     df: number of selected coefficients at selected lambda
  #     selected_variables: dataframe of selected regressors and coefficients
  #     n_selected: number of selected coefficients
  #     n_selected_groups: number of selected groups
  #     target_name: name of the target variable
  #     date_new: date corresponding to the nowcast
  #     last_obs_y: last observed quarterly target
  #     fit: full tscv.sglfit object
  # -------------------------------------------------------------------------
  
  # ---- STEP 1: Preparation, building direct MIDAS lag structure -
  # Monthly lag blocks for all predictors and quarterly lags of y.
  midas_lags <- build_direct_midas_lags(
    df_input = df_input,
    x_lags   = x_lags,
    y_lags   = y_lags
  )
  
  # Legendre polynomial transformation 
  # Each predictor block of monthly lags is compressed/projected onto
  # a Legendre basis. This reduces the dimensionality of each MIDAS block.
  midas_legendre <- build_legendre_midas_blocks(
    midas_lags_obj  = midas_lags,
    legendre_degree = legendre_degree,
    penalize_y_lags = penalize_y_lags
  )
  
  # Extract estimation data, nowcast row, target and penalty structure
  X_est <- midas_legendre$X_est
  X_new <- midas_legendre$X_new
  y_est <- midas_legendre$y_est
  gindex <- midas_legendre$gindex
  pf <- midas_legendre$pf
  
  # Making sure everything has compatible dimensions
  stopifnot(nrow(X_est) == length(y_est))
  stopifnot(ncol(X_new) == ncol(X_est))
  stopifnot(length(gindex) == ncol(X_est))
  stopifnot(length(pf) == ncol(X_est))
  
  # Keep only rows where both the target and all regressors are finite.
  valid_rows <- is.finite(y_est) & apply(X_est, 1, function(z) all(is.finite(z)))
  X_est <- X_est[valid_rows, , drop = FALSE]
  y_est <- y_est[valid_rows]
  
  
  # Constant regressors cannot be standardized and are not informative
  # for the penalized regression, so we remove them
  valid_cols <- apply(X_est, 2, function(z) all(is.finite(z)) && sd(z, na.rm = TRUE) > 0)
  X_est <- X_est[, valid_cols, drop = FALSE]
  X_new <- X_new[, valid_cols, drop = FALSE]
  gindex <- gindex[valid_cols]
  pf <- pf[valid_cols]
  
  # Re-index groups after column filtering so that group labels are consecutive
  gindex <- as.integer(factor(gindex))
  
  # Standardization
  # Scaling is estimated only on the estimation sample and then applied to X_new.
  # This avoids forward-looking bias from the nowcast row.
  # The pseudo-real time framework is such that everytime we estimate the model,
  # those statistics do not incoporate any "future" information (like if we were in the past).
  x_center <- colMeans(X_est, na.rm = TRUE)
  x_scale  <- apply(X_est, 2, sd, na.rm = TRUE)
  x_scale[!is.finite(x_scale) | x_scale == 0] <- 1
  
  X_est_scaled <- scale(X_est, center = x_center, scale = x_scale)
  X_new_scaled <- scale(X_new, center = x_center, scale = x_scale)
  
  y_center <- mean(y_est, na.rm = TRUE)
  y_scale  <- sd(y_est, na.rm = TRUE)
  
  if (!is.finite(y_scale) || y_scale == 0) {
    stop("y_est has zero or non-finite standard deviation.")
  }
  
  y_est_scaled <- as.numeric(scale(y_est, center = y_center, scale = y_scale))
  
  # ---- STEP 2: ESTIMATION ---- 
  # Time series cross validation setting
  # If K is not provided, we use a conservative value based on sample size.
  if (is.null(K)) {
    K <- min(10, floor(nrow(X_est_scaled) / 3))
  }
  
  if (K < 2) {
    stop("Not enough observations for time-series cross-validation.")
  }
  
  # Estimation of a Sparse-Group LASSO MIDAS
  # standardize = FALSE because scaling has already been done manually.
  # tscv = takes into account the time series setting 
  fit_sgl <- midasml::tscv.sglfit(
    x = as.matrix(X_est_scaled),
    y = as.numeric(y_est_scaled),
    gindex = gindex,
    gamma = gamma,
    K = K,
    l = l,
    standardize = FALSE,
    intercept = TRUE,
    pf = pf
  )
  
  # Cross validation to choose the value of the hyperparameter lambda
  lambda_min <- fit_sgl$lambda[which.min(fit_sgl$cvm)]
  lambda_id  <- which.min(abs(fit_sgl$sgl.fit$lambda - lambda_min))
  
  # ---- STEP 3: OOS nowcast ---- 
  # Predict on the standardized scale, then convert back to the original scale.
  pred_scaled <- predict(
    fit_sgl$sgl.fit,
    newx = as.matrix(X_new_scaled),
    s = lambda_min,
    method = "single"
  )
  
  pred <- as.numeric(pred_scaled) * y_scale + y_center
  
  # Selected variables: we extract non-zero coefficients at the selected lambda.
  beta_hat <- as.numeric(fit_sgl$sgl.fit$beta[, lambda_id])
  selected_id <- which(abs(beta_hat) > 1e-8)
  
  selected_variables <- data.frame(
    column = colnames(X_est_scaled)[selected_id],
    group = gindex[selected_id],
    pf = pf[selected_id],
    coef_scaled = beta_hat[selected_id],
    row.names = NULL
  )
  
  # Return results 
  return(list(
    nowcast = pred,
    lambda = lambda_min,
    gamma = gamma,
    cv_mse = min(fit_sgl$cvm),
    df = fit_sgl$sgl.fit$df[lambda_id],
    selected_variables = selected_variables,
    n_selected = length(selected_id),
    n_selected_groups = length(unique(gindex[selected_id])),
    target_name = colnames(df_input)[2],
    date_new = midas_lags$date_new,
    last_obs_y = midas_lags$last_obs_y,
    fit = fit_sgl
  ))
}



build_direct_midas_lags <- function(df_input, x_lags = 2, y_lags = 1) {
  # =========================================================================
  # DESCRIPTION
  # Function to build the raw MIDAS lag structure for direct MIDAS models.
  # The target is observed quarterly, while predictors are observed monthly.
  # For each observed quarter, the function builds one row containing the
  # current and lagged monthly values of each predictor, plus optional quarterly
  # lags of the target.
  # -------------------------------------------------------------------------
  # INPUTS
  #     df_input: dataframe containing the series. Dates should be in the first
  #               column, target variable in the second column, and predictors
  #               in the remaining columns.
  #     x_lags: number of monthly lags used for each predictor.
  #             ex: if = 2, take current month, previous month and month before.
  #     y_lags: number of quarterly autoregressive lags of the target.
  #
  # OUTPUTS
  #     y_est: quarterly target values used for estimation
  #     X_exog_est: raw MIDAS lag matrix for monthly predictors
  #     X_exog_new: raw MIDAS lag row for the nowcast
  #     X_y_lags_est: matrix of quarterly y lags for estimation
  #     X_y_lags_new: row of quarterly y lags for the nowcast
  #     q_est_idx: month indices corresponding to estimation quarters
  #     x_names: names of monthly predictors
  #     x_lags: number of monthly predictor lags
  #     y_lags: number of quarterly target lags
  #     date_new: date corresponding to the nowcast row
  #     last_obs_q: index of the last observed quarterly target
  #     last_obs_y: value of the last observed quarterly target
  # -------------------------------------------------------------------------
  
  # ---- STEP 1 : Cleaning ----
  # Remove columns that are entirely missing.
  df <- df_input[, colSums(is.na(df_input)) < nrow(df_input)]
  
  # Dates are assumed to be in the first column, target in the second column.
  dates <- as.Date(df[[1]])
  Y_raw <- as.numeric(df[[2]])
  X_raw <- df[, -c(1, 2), drop = FALSE]
  
  # Fill missing predictor values 
  X_filled <- zoo::na.locf(X_raw, na.rm = FALSE, fromLast = FALSE)
  X_filled <- zoo::na.locf(X_filled, na.rm = FALSE, fromLast = TRUE)
  X_filled <- as.matrix(X_filled)
  
  # The quarterly target is assumed to be observed every third monthly row.
  n_months_total <- nrow(X_filled)
  n_q_total <- floor(n_months_total / 3)
  
  q_end_idx <- seq(3, n_q_total * 3, by = 3)
  y_all <- Y_raw[q_end_idx]
  
  # Identify the last quarter with an observed target.
  non_na_idx <- which(!is.na(y_all))
  if (length(non_na_idx) == 0) {
    stop("No non-NA quarterly target values found.")
  }
  
  last_obs_q <- max(non_na_idx)
  
  # Keep only quarters up to the last observed target.
  y_all_obs <- y_all[1:last_obs_q]
  q_end_obs <- q_end_idx[1:last_obs_q]
  q_pos_obs <- seq_along(y_all_obs)
  
  # Keep valid estimation rows 
  # A row is valid if:
  #   1. enough monthly lags are available,
  #   2. the quarterly target is observed,
  #   3. enough quarterly y lags are available.
  valid_rows <- which(
    q_end_obs - x_lags >= 1 &
      !is.na(y_all_obs) &
      q_pos_obs > y_lags
  )
  
  if (length(valid_rows) == 0) {
    stop("No valid rows after applying x_lags and y_lags.")
  }
  
  y_est <- y_all_obs[valid_rows]
  q_est_idx <- q_end_obs[valid_rows]
  q_pos <- q_pos_obs[valid_rows]
  
  
  # STEP 2: build the dataset
  # For a given month index, collect lag 0 to lag x_lags for each predictor.
  build_one_exog_row <- function(month_idx) {
    # Helper to build one row of monthly predictor lags
    as.numeric(unlist(lapply(seq_len(ncol(X_filled)), function(k) {
      X_filled[month_idx - 0:x_lags, k]
    })))
  }
  
  # For a given quarter index, collect previous quarterly target values.
  build_one_y_lag_row <- function(q_index) {
    # Helper to build one row of quarterly target lags
    if (y_lags == 0) return(numeric(0))
    as.numeric(y_all[q_index - seq_len(y_lags)])
  }
  
  # Estimation design matrix for monthly predictors 
  X_exog_est <- do.call(rbind, lapply(q_est_idx, build_one_exog_row))
  
  x_names <- colnames(X_filled)
  exog_col_names <- unlist(lapply(x_names, function(v) {
    paste0(v, "_lag", 0:x_lags)
  }))
  colnames(X_exog_est) <- exog_col_names
  
  # Add autoregressive quarterly y lags if requested
  if (y_lags > 0) {
    X_y_lags_est <- do.call(rbind, lapply(q_pos, build_one_y_lag_row))
    X_y_lags_est <- as.matrix(X_y_lags_est)
    colnames(X_y_lags_est) <- paste0("Y_lag", seq_len(y_lags))
  } else {
    X_y_lags_est <- NULL
  }
  
  # The nowcast row uses the last available monthly observation.
  idx_new <- n_months_total
  
  if (idx_new - x_lags < 1) {
    stop("Not enough monthly observations to build X_new.")
  }
  
  X_exog_new <- matrix(build_one_exog_row(idx_new), nrow = 1)
  colnames(X_exog_new) <- exog_col_names
  
  # Add y lags for the nowcast row
  if (y_lags > 0) {
    if (last_obs_q < y_lags) {
      stop("Not enough observed quarterly y values to build y lags for X_new.")
    }
    
    X_y_lags_new <- matrix(
      as.numeric(y_all[last_obs_q - 0:(y_lags - 1)]),
      nrow = 1
    )
    colnames(X_y_lags_new) <- paste0("Y_lag", seq_len(y_lags))
  } else {
    X_y_lags_new <- NULL
  }
  
  # Return MIDAS lag object
  return(list(
    y_est          = y_est,
    X_exog_est     = X_exog_est,
    X_exog_new     = X_exog_new,
    X_y_lags_est   = X_y_lags_est,
    X_y_lags_new   = X_y_lags_new,
    q_est_idx      = q_est_idx,
    x_names        = x_names,
    x_lags         = x_lags,
    y_lags         = y_lags,
    date_new       = dates[idx_new],
    last_obs_q     = last_obs_q,
    last_obs_y     = y_all[last_obs_q]
  ))
}




build_legendre_midas_blocks <- function(midas_lags_obj,
                                        legendre_degree = 0,
                                        penalize_y_lags = FALSE) {
  # =========================================================================
  # DESCRIPTION
  # Function to transform raw MIDAS lag blocks using Legendre polynomials.
  # Each predictor is initially represented by a block of monthly lags. This
  # function projects each lag block onto a Legendre basis, creating one group
  # per original predictor for sparse-group LASSO estimation.
  # -------------------------------------------------------------------------
  # INPUTS
  #     midas_lags_obj: output from build_direct_midas_lags.
  #     legendre_degree: degree of the Legendre polynomial basis.
  #                      degree = 0 gives one basis coefficient per predictor.
  #     penalize_y_lags: logical. If FALSE, autoregressive y lags receive a
  #                      penalty factor equal to zero and are not penalized.
  #
  # OUTPUTS
  #     y_est: quarterly target values used for estimation
  #     X_est: transformed estimation matrix
  #     X_new: transformed nowcast row
  #     gindex: group index for sparse-group LASSO
  #     pf: penalty factor vector
  #     W: Legendre basis matrix
  #     x_names: names of monthly predictors
  #     x_lags: number of monthly predictor lags
  #     y_lags: number of quarterly target lags
  #     degree: Legendre polynomial degree
  #     raw_object: original raw MIDAS lag object
  # -------------------------------------------------------------------------
  
  # ---- STEP 1: Setup ----
  # Extract raw MIDAS objects
  X_exog_est_raw <- midas_lags_obj$X_exog_est
  X_exog_new_raw <- midas_lags_obj$X_exog_new
  X_y_lags_est   <- midas_lags_obj$X_y_lags_est
  X_y_lags_new   <- midas_lags_obj$X_y_lags_new
  
  x_names <- midas_lags_obj$x_names
  x_lags  <- midas_lags_obj$x_lags
  y_lags  <- midas_lags_obj$y_lags
  
  # Number of monthly observations in each MIDAS lag block
  jmax <- x_lags + 1
  
  # The raw exogenous matrix should contain one lag block of length jmax
  # for each monthly predictor.
  if (ncol(X_exog_est_raw) != length(x_names) * jmax) {
    stop("Inconsistent dimensions for exogenous MIDAS blocks.")
  }
  
  # ---- STEP 2: Legendre ---- 
  # W maps raw monthly lag blocks into lower-dimensional polynomial features.
  W <- midasml::lb(
    degree = legendre_degree,
    a      = 0,
    b      = 1,
    jmax   = jmax
  )
  
  W <- as.matrix(W)
  
  # Ensure that rows of W correspond to monthly lags.
  if (nrow(W) != jmax && ncol(W) == jmax) {
    W <- t(W)
  }
  
  if (nrow(W) != jmax) {
    stop("Legendre matrix W has incompatible dimensions.")
  }
  
  # Containers for transformed blocks and penalty/group information
  X_est_blocks <- list()
  X_new_blocks <- list()
  new_colnames <- c()
  gindex <- c()
  pf <- c()
  
  current_group <- 1
  
  # Add autoregressive y lags
  # y lags are treated as their own group. They can be penalized or left
  # unpenalized depending on penalize_y_lags.
  if (y_lags > 0) {
    X_est_blocks[[length(X_est_blocks) + 1]] <- X_y_lags_est
    X_new_blocks[[length(X_new_blocks) + 1]] <- X_y_lags_new
    
    new_colnames <- c(new_colnames, colnames(X_y_lags_est))
    
    gindex <- c(gindex, rep(current_group, y_lags))
    
    if (penalize_y_lags) {
      pf <- c(pf, rep(1, y_lags))
    } else {
      pf <- c(pf, rep(0, y_lags))
    }
    
    current_group <- current_group + 1
  }
  
  # ---- STEP 3: Groupping ----
  # Each original predictor gets its own group in the sparse-group LASSO.
  for (k in seq_along(x_names)) {
    
    # Select the raw monthly lag block for predictor k.
    cols_k <- ((k - 1) * jmax + 1):(k * jmax)
    
    Z_est_k <- X_exog_est_raw[, cols_k, drop = FALSE]
    Z_new_k <- X_exog_new_raw[, cols_k, drop = FALSE]
    
    # Project raw lag block onto the Legendre basis.
    X_est_k <- Z_est_k %*% W
    X_new_k <- Z_new_k %*% W
    
    X_est_blocks[[length(X_est_blocks) + 1]] <- X_est_k
    X_new_blocks[[length(X_new_blocks) + 1]] <- X_new_k
    
    n_basis_k <- ncol(X_est_k)
    
    # Name Legendre-transformed columns.
    new_colnames <- c(
      new_colnames,
      paste0(x_names[k], "_leg", seq_len(n_basis_k) - 1)
    )
    
    # Same group for all Legendre coefficients of the same predictor.
    gindex <- c(gindex, rep(current_group, n_basis_k))
    
    # Exogenous predictors are penalized.
    pf <- c(pf, rep(1, n_basis_k))
    
    current_group <- current_group + 1
  }
  
  # Combine all blocks 
  X_est <- do.call(cbind, X_est_blocks)
  X_new <- do.call(cbind, X_new_blocks)
  
  colnames(X_est) <- new_colnames
  colnames(X_new) <- new_colnames
  
  # Return transformed MIDAS object 
  return(list(
    y_est       = midas_lags_obj$y_est,
    X_est       = X_est,
    X_new       = X_new,
    gindex      = gindex,
    pf          = pf,
    W           = W,
    x_names     = x_names,
    x_lags      = x_lags,
    y_lags      = y_lags,
    degree      = legendre_degree,
    raw_object  = midas_lags_obj
  ))
}