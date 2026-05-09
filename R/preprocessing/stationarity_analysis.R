ADF_series <- function(ts_series, type_1_error = "p5"){
  # Run the sequential strategy to assess whether a series is stationary
  # using the augmented Dickey-Fuller non-stationarity test
  # '
  # @param ts_series    : time series for which we want to 
  #                       assess stationarity
  # @param type_1_error : type I error level 
  #                       (either p1, p5 or p10)
  
  # Initialisation
  
  n_obs = length(ts_series)
  alpha <- switch(type_1_error,
                  "p1"  = 0.01,
                  "p5"  = 0.05,
                  "p10" = 0.10,
                  stop("type_1_error must be one of: 
                       p1, p5, p10"))
  cval_col <- switch(type_1_error,
                     "p1" = "1pct",
                     "p5" = "5pct",
                     "p10" = "10pct")
  
  # 1. Trend and intercept 
  ur_df_trend <- ur.df(ts_series, type = "trend", selectlags = "AIC")
  # Dickey-Fuller statistic
  trend_stat <- ur_df_trend@teststat[, "tau3"]
  # Dickey-Fuller test critical value
  trend_crit <- ur_df_trend@cval["tau3", cval_col]
  # T-stat for the trend
  trend_tstat <- ur_df_trend@testreg$coefficients["tt", "t value"]
  
  # If we accept the null hypothesis, test statistic of significance
  # does not follow a Student law, so need to compare to Dickey-Fuller
  # computed critical values
  if (trend_stat > trend_crit){
    
    # Unit root so the t-stat doesn't follow a Student law
    cval_signif = get_DF_crit_val_signif("model3_trend", 
                                         n_obs, type_1_error)
    
    if (abs(trend_tstat) > cval_signif){
      return("I(1) + c + T")
    }
  } else {
    # No unit root so t-stat follows a Student law
    df_resid    <- ur_df_trend@testreg$df[2]
    cval_signif <- qt(1 - alpha/2, df = df_resid)
    if (abs(trend_tstat) > cval_signif){
      return("I(0) + c + T")}
  } 
  
  # 2. Drift only
  ur_df_drift <- ur.df(ts_series, type = "drift", selectlags = "AIC")
  drift_stat  <- ur_df_drift@teststat[, "tau2"]
  drift_crit  <- ur_df_drift@cval["tau2", cval_col]
  drift_tstat <- ur_df_drift@testreg$coefficients["(Intercept)", "t value"]
  
  if (drift_stat > drift_crit){
    cval_signif <- get_DF_crit_val_signif("model2_const", 
                                          n_obs, type_1_error)
    if (abs(drift_tstat) > cval_signif) {
      return("I(1) + c") }
  } else {
    df_resid    <- ur_df_drift@testreg$df[2]
    cval_signif <- qt(1 - alpha/2, df = df_resid)
    if (abs(drift_tstat) > cval_signif) {
      return("I(0) + c") }
  }
  
  # 3. No drift nor trend
  ur_df_none <- ur.df(ts_series, type = "none", selectlags = "AIC")
  none_stat  <- ur_df_none@teststat[, "tau1"]
  none_crit  <- ur_df_none@cval["tau1", cval_col]
  
  if (none_stat > none_crit) {
    return("I(1)")}
  else {
    return("I(0)")}
}

get_results_ADF_per_country <- function(country_list) {
  # For a given structure data_country (with Revenues and Expenditures),
  # returns the ADF result for each regressor for each block.
  # '
  # @param country_list: list of 2 tables (Revenues and Expenditures)
  
  diagnose_sheet <- function(df) {
    # Sub-function that process the dataframe and run the test
    # on clean series
    
    # Exclude dates
    numeric_cols <- df[sapply(df, is.numeric)]
    # 2. ADF test for each variable
    results <- sapply(names(numeric_cols), function(col_name) {
      
      # Remove NA otherwise not possible to run ADF test
      series_clean <- na.omit(numeric_cols[[col_name]])
      # We also make sure that we have minimal data
      if (length(series_clean) < 10) {
        return("Insufficient Data")
      }
      # ADF test
      return(ADF_series(series_clean))
    })
    
    # 3. Formating
    return(data.frame(
      Series = names(results),
      Status = results,
      row.names = NULL,
      stringsAsFactors = FALSE
    ))
  }
  
  # We do this procedure for both blocks
  output_list <- list(
    Revenues = diagnose_sheet(country_list$Revenues),
    Expenditures = diagnose_sheet(country_list$Expenditures)
  )
  
  return(output_list)
}
