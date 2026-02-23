get_DF_crit_val_signif <- function(table_col, n_obs, level = "p5") {
  # Function returning the corresponding critical value for significance test
  # of trend and / or drift when the series is not stationary.
  # Model 2 = drift only ; model 3 = drift + trend
  # '
  # @param table_col : either model2_const, model3_const or model3_trend
  # @param n_obs : number of observations
  # @param level : type I error level (either p1, p5 or p10)
  
  # Dickey-Fuller tabulated values 
  df_table <- list(
    
    # Model [2] : intercept only
    model2_const = data.frame(
      n    = c(25, 50, 100, 250, 500, Inf),
      p1   = c(3.41, 3.28, 3.22, 3.19, 3.18, 3.18),
      p5   = c(2.97, 2.89, 2.86, 2.84, 2.83, 2.83),
      p10  = c(2.61, 2.56, 2.54, 2.53, 2.52, 2.52)
    ),
    
    # Model [3] : trend and drift
    model3_const = data.frame(
      n    = c(25, 50, 100, 250, 500, Inf),
      p1   = c(4.05, 3.87, 3.78, 3.74, 3.72, 3.71),
      p5   = c(3.59, 3.47, 3.42, 3.39, 3.38, 3.38),
      p10  = c(3.20, 3.14, 3.11, 3.09, 3.08, 3.08)
    ),
    
    model3_trend = data.frame(
      n    = c(25, 50, 100, 250, 500, Inf),
      p1   = c(3.74, 3.60, 3.53, 3.49, 3.48, 3.46),
      p5   = c(3.25, 3.18, 3.14, 3.12, 3.11, 3.11),
      p10  = c(2.85, 2.81, 2.79, 2.79, 2.78, 2.78)
    )
  )

  ns <- df_table[[table_col]]$n
  cvals <- df_table[[table_col]][[level]]
  # We take the critical value corresponding to the 
  # asked Type I error and for the number of observation that 
  # is the closest to the one of the series
  if (n_obs >= max(ns[is.finite(ns)])) {
    idx <- which(is.infinite(ns) | ns == max(ns))
  } else {
    idx <- which.min(abs(ns - n_obs))
  }
  return(cvals[idx])
}


