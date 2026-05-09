ar1_model <- function(df_input){
  # =========================================================================
  # DESCRIPTION
  # Function to estimate an AR(1) model for nowcasting
  #
  # -------------------------------------------------------------------------
  # INPUTS
  #     df_input: dataframe containing the series. Target variable should be in second 
  #               column (dates in first). 
  #
  # OUTPUTS
  #     model: results of the AR(1) estimation
  #     y_target: aligned quarterly target variable
  #     nb_factors: NA
  #     factors: NA
  #     nowcast: out-of-sample nowcast for the current quarter
  #     date_new: date corresponding to the nowcast
  #     last_obs_y: last observed quarterly target
  # -------------------------------------------------------------------------
  
  # Cleaning 
  df <- df_input[, colSums(is.na(df_input)) < nrow(df_input)]
  
  # Target is supposed to be the second column, Dates in first
  dates <- as.Date(df[[1]])
  Y_raw <- as.numeric(df[, 2])
  
  # Separate estimation and nowcast periods 
  n_months_total <- nrow(df)
  
  # Build quarterly target from every 3rd month
  n_q_total <- floor(n_months_total / 3)
  y_all <- Y_raw[seq(3, n_q_total * 3, by = 3)]
  
  # Find last non-NA quarter (last quarter with known target)
  non_na_idx <- which(!is.na(y_all))
  if (length(non_na_idx) == 0) stop("No non-NA quarterly target values found")
  last_obs_q <- max(non_na_idx)
  
  # Estimation sample = only quarters with known target
  y_est <- y_all[1:last_obs_q]
  y_ts  <- ts(as.numeric(scale(y_est)), frequency = 4)
  
  # AR(1) Estimation
  y_curr <- as.numeric(y_ts)[2:length(y_ts)]
  y_lag  <- as.numeric(y_ts)[1:(length(y_ts)-1)]
  fit <- lm(y_curr ~ y_lag) # since data is standardized, no need to include a constant
  
  # OOS nowcast = rho*y(t-1)
  last_y <- tail(as.numeric(y_ts), 1)
  
  if(is.na(coef(fit)[1])){
    # Handle edge case where we don't have enough data
    nowcast_val <- last_y
  } else {
    nowcast_val <- as.numeric(coef(fit)[1] + coef(fit)[2] * last_y)
  }
  
  # Restore to original scale
  y_mean <- mean(y_est, na.rm = TRUE)
  y_sd   <- sd(y_est, na.rm = TRUE)
  nowcast_val <- nowcast_val * y_sd + y_mean
  
  return(list(
    model      = fit,
    y_target   = y_ts,
    nb_factors = NA,
    factors    = NA,
    nowcast    = nowcast_val,
    date_new   = dates[n_months_total],
    last_obs_y = y_all[last_obs_q]
  ))
}

rw_model <- function(df_input){
  # =========================================================================
  # DESCRIPTION
  # Function to estimate a Random Walk model for nowcasting
  # Globally its structure is the same as AR(1) function
  # -------------------------------------------------------------------------
  # INPUTS
  #     df_input: dataframe containing the series. Target variable should be in second 
  #               column (dates in first). 
  #
  # OUTPUTS
  #     model: character string "Random Walk"
  #     y_target: aligned quarterly target variable
  #     nb_factors: NA
  #     factors: NA
  #     nowcast: out-of-sample nowcast for the current quarter
  #     date_new: date corresponding to the nowcast
  #     last_obs_y: last observed quarterly target
  # -------------------------------------------------------------------------
  
  # Cleaning 
  df <- df_input[, colSums(is.na(df_input)) < nrow(df_input)]
  
  # Target is supposed to be the second column, Dates in first
  dates <- as.Date(df[[1]])
  Y_raw <- as.numeric(df[, 2])
  
  # Separate estimation and nowcast periods 
  n_months_total <- nrow(df)
  
  # Build quarterly target from every 3rd month
  n_q_total <- floor(n_months_total / 3)
  y_all <- Y_raw[seq(3, n_q_total * 3, by = 3)]
  
  # Find last non-NA quarter (last quarter with known target)
  non_na_idx <- which(!is.na(y_all))
  if (length(non_na_idx) == 0) stop("No non-NA quarterly target values found")
  last_obs_q <- max(non_na_idx)
  
  # Estimation sample: only quarters with known target
  y_est <- y_all[1:last_obs_q]
  y_ts  <- ts(as.numeric(scale(y_est)), frequency = 4)
  
  # Restore to original scale
  y_mean <- mean(y_est, na.rm = TRUE)
  y_sd   <- sd(y_est, na.rm = TRUE)
  
  # OOS nowcast = last known value
  nowcast_val_scaled <- tail(as.numeric(y_ts), 1)
  nowcast_val <- nowcast_val_scaled * y_sd + y_mean
  
  return(list(
    model      = "Random Walk",
    y_target   = y_ts,
    nb_factors = NA,
    factors    = NA,
    nowcast    = nowcast_val,
    date_new   = dates[n_months_total],
    last_obs_y = y_all[last_obs_q]
  ))
}
