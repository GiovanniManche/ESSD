pseudo_real_time <- function(df_ragged, start_date, method = "expanding",
                             n_window = 36, kmax = 3, lags_factors = 2){
  
  
  # Get dates for which we should nowcast
  all_dates <- as.Date(df_ragged[[1]]) 
  start_dt  <- as.Date(start_date)
  dates_nowcast <- all_dates[all_dates >= as.Date(start_date)]
  
  nowcast_results <- data.frame(
    publication_date = character(),
    nowcast_value    = numeric(),
    nb_factors       = numeric()
  )
  
  cat("Nowcasting starts\n")
  for (i in 1:length(dates_nowcast)){
    # Get vintage data for the given date
    current_date = dates_nowcast[i]
    n_val <- if(method == "expanding") nrow(df_ragged) else n_window
    vintage_data <- available_data(df_ragged,
                                   n = n_val,
                                   target_date = current_date,
                                   method = method)
    
    # Estimation
    model_t <- try(factor_midas(vintage_data, 
                                kmax = kmax, 
                                lags_factors = lags_factors), 
                   silent = TRUE)
    
    # If estimation went well: stock results
    if (!inherits(model_t, "try-error")){
      # Use out-of-sample nowcast (computed in factor_midas)
      current_nowcast <- model_t$nowcast
      
      new_results <- data.frame(
        publication_date = as.character(current_date),
        nowcast_value    = as.numeric(current_nowcast),
        nb_factors       = model_t$nb_factors
      )
      nowcast_results <- rbind(nowcast_results, new_results)
      cat("[OK] Date:", as.character(current_date), "Nb factors:", model_t$nb_factors,
          "Nowcast:", round(as.numeric(current_nowcast), 4), "\n")
    }
    else{
      cat("[KO]: Error at date", as.character(current_date), "Reason:", 
          attr(model_t, "condition")$message, "\n")
    }
  }
  return (nowcast_results)
  
}
