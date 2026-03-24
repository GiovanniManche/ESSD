diff_sparse <- function(x) {
  out <- rep(NA, length(x))
  idx <- which(!is.na(x))
  
  if (length(idx) > 1) {
    gaps <- diff(idx)  
    
    for (k in 2:length(idx)) {
      gap <- idx[k] - idx[k-1]
      
      if (gap == 1 || gap == 3) {
        out[idx[k]] <- x[idx[k]] - x[idx[k-1]]
      } else {
        out[idx[k]] <- NA
      }
    }
  }
  
  return(out)
}

stationarize_data <- function(df, diag_df) {
  # Function to stationarize series given their ADF diagnostic.
  # If there is a trend and / or a constant, we regress the series
  # on time and take the residuals. 
  # If the series is I(1), we differenciate it (or the aforementioned residuals)
  # '
  # @param df : dataframe with the series (typically, data_country$Revenues)
  # @param diag_df : corresponding dataframe with the results of the sequential 
  #                  strategy for each variable (result of get_results_ADF_per_country)
  
  
  df_stat <- df
  
  # We create a time vector
  n_obs <- nrow(df)
  t <- 1:n_obs
  
  # For each variable in the diagnostic table (organized in rows)
  for (i in 1:nrow(diag_df)) {
    col_name <- diag_df$Series[i]
    status   <- diag_df$Status[i]
    
    # We check that the series name corresponds to a real series 
    if (col_name %in% names(df)) {
      x <- df[[col_name]]
      
      if (status == "I(0)") {
        # Already stationary  
        x_statio <- x
        
      } else if (status == "I(0) + c"|| status == "I(0) + c + T") {
        # Remove the deterministic component
        mod <- lm(x ~ t, na.action = na.exclude)
        x_statio <- residuals(mod)
        
      } else if (status == "I(1)") {
        # Integrated so differentiate
        x_statio <- diff_sparse(x)
        
      } else if (status == "I(1) + c" || status == "I(1) + c + T") {
        # First remove the deterministic component
        # Then differentiate
        mod <- lm(x ~ t, na.action = na.exclude)
        res <- residuals(mod)
        x_statio <- diff_sparse(res)
        
      } else {
        x_statio <- rep(NA, n_obs)
      }
      
      # On remplace la colonne brute par la colonne stationnarisée
      df_stat[[col_name]] <- x_statio
    }
  }
  
  return(df_stat)
}

stationarize_country <- function(country_data, country_diag) {
  list(
    Revenues     = stationarize_data(country_data$Revenues, country_diag$Revenues),
    Expenditures = stationarize_data(country_data$Expenditures, country_diag$Expenditures)
  )
}