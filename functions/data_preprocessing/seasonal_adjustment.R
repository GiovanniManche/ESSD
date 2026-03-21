seasonally_adjust_series <- function(x, dates) {
  # Seasonally adjust a monthly series using X-13ARIMA-SEATS.
  # '
  # @param x     : numeric vector (monthly, may contain NAs)
  # @param dates : corresponding Date vector (for the ts() object)
  # @return      : seasonally adjusted series (same length as x, NAs preserved
  #                at positions where x was originally NA)
  
  na_positions <- is.na(x)
  # X-13 needs at least 3 full years of data
  if (sum(!na_positions) < 36) {
    warning("Not enough observations for X-13, returning series as-is.")
    return(x)
  }
  
  # Anchor ts() to the calendar using the first non-NA date
  first_date  <- min(dates[!na_positions])
  start_year  <- as.integer(format(first_date, "%Y"))
  start_month <- as.integer(format(first_date, "%m"))
  
  # Interpolate NAs linearly so X-13 receives a complete series
  x_interp <- zoo::na.approx(x, na.rm = FALSE)
  x_interp <- zoo::na.locf(x_interp, na.rm = FALSE)
  x_interp <- zoo::na.locf(x_interp, fromLast = TRUE, na.rm = FALSE)
  
  ts_x <- ts(x_interp, start = c(start_year, start_month), frequency = 12)
  
  # Run X-13ARIMA-SEATS
  x13_fit <- tryCatch(
    seasonal::seas(ts_x, transform.function = "none"),
    error = function(e) {
      warning(paste("X-13 failed:", conditionMessage(e), ", returning series as-is."))
      return(NULL)
    }
  )
  
  if (is.null(x13_fit)) return(x)
  # Extract seasonally adjusted component 
  x_adjusted <- as.numeric(seasonal::final(x13_fit))
  # Restore original NAs
  x_adjusted[na_positions] <- NA
  
  return(x_adjusted)
}

seasonally_adjust_data <- function(df) {
  # Apply X-13 to all numeric series in a dataframe.
  #
  # @param df : dataframe with series (e.g. data_country$Revenues)
  #             first column (non-numeric) = dates
  
  df_sa <- df
  
  # Identify the date column
  date_col <- names(df)[!sapply(df, is.numeric)][1]
  dates    <- df[[date_col]]
  
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  
  for (col_name in numeric_cols) {
    cat("  Adjusting:", col_name, "\n")
    df_sa[[col_name]] <- seasonally_adjust_series(df[[col_name]], dates)
  }
  
  return(df_sa)
}

seasonally_adjust_country <- function(country_data) {
  # Country-level wrapper
  # @param country_data : list(Revenues = df, Expenditures = df)
  
  cat("Revenues:\n")
  rev_sa <- seasonally_adjust_data(country_data$Revenues)
  cat("Expenditures:\n")
  exp_sa <- seasonally_adjust_data(country_data$Expenditures)
  
  list(
    Revenues     = rev_sa,
    Expenditures = exp_sa
  )
}
