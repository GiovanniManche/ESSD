decumulate_series <- function(x, dates) {
  # Convert a cumulative monthly series into a flow series
  #'
  # @param x     : numeric vector (cumulative, may contain NAs)
  # @param dates : corresponding Date vector (used to identify January)
  # @return      : monthly flow series
  
  months   <- as.integer(format(dates, "%m"))
  x_flow   <- rep(NA_real_, length(x))
  
  for (i in seq_along(x)) {
    if (is.na(x[i])) next
    
    if (months[i] == 1) {
      # January = already a flow
      x_flow[i] <- x[i]
    } else {
      # All other months: diff within the fiscal year
      # If previous observation is NA (missing month), leave as NA
      if (!is.na(x[i - 1])) {
        x_flow[i] <- x[i] - x[i - 1]
      }
    }
  }
  
  return(x_flow)
}

decumulate_data <- function(df, col_names) {
  # Apply decumulation to a specified set of columns in a dataframe.
  # 
  # @param df        : dataframe with series (e.g. data_country$Revenues)
  #                    
  # @param col_names : character vector of column names to decumulate
  #                    (ex: c("Cash revenues", "Cash expenditures"))
  
  date_col <- names(df)[!sapply(df, is.numeric)][1]
  dates    <- df[[date_col]]
  
  df_flow <- df
  
  for (col_name in col_names) {
    if (!col_name %in% names(df)) {
      warning(paste("Column not found, skipping:", col_name))
      next
    }
    cat("  Decumulating:", col_name, "\n")
    df_flow[[col_name]] <- decumulate_series(df[[col_name]], dates)
  }
  
  return(df_flow)
}

decumulate_country <- function(country_data,
                               rev_cols = "Cash revenues",
                               exp_cols = "Cash expenditures") {
  # Country-level wrapper
  #
  # @param country_data : list(Revenues = df, Expenditures = df)
  # @param rev_cols     : column name(s) to decumulate in Revenues sheet
  # @param exp_cols     : column name(s) to decumulate in Expenditures sheet
  
  cat("Revenues:\n")
  rev_flow <- decumulate_data(country_data$Revenues,     col_names = rev_cols)
  cat("Expenditures:\n")
  exp_flow <- decumulate_data(country_data$Expenditures, col_names = exp_cols)
  
  list(
    Revenues     = rev_flow,
    Expenditures = exp_flow
  )
}