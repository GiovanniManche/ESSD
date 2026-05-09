#' Decumulate a cumulative(year-to-date) time series
#'
#' Converts a monthly cumulative series (e.g., year-to-date fiscal data) into 
#' a flow series. The decumulation resets every January.
#' 
#' @param x Numeric vector. The cumulative series (can contain NAs).
#' @param dates Date vector. Corresponding dates used to identify January.
#' 
#' @return A numeric vector of the same length as \code{x} containing the monthly flows.
#' 
#' @examples
#' my_dates <- seq(as.Date("2023-01-01"), as.Date("2023-06-01"), by = "month")
#' my_cum_data <- c(10, 25, 40, NA, 70, 85) # March=40, April=NA, May=70
#' 
#' decumulate_series(my_cum_data, my_dates)
#' # Expected: 10, 15, 15, NA, NA, 15
#' 
#' @export 
decumulate_series <- function(x, dates) {
  months <- as.integer(format(dates, "%m"))
  x_flow <- rep(NA_real_, length(x))
  
  for (i in seq_along(x)) {
    if (is.na(x[i])) next
    
    if (months[i] == 1) {
      x_flow[i] <- x[i]
    } else {
      if (i > 1 && !is.na(x[i - 1])) {
        x_flow[i] <- x[i] - x[i - 1]
      }
    }
  }
  return(x_flow)
}

#' Apply decumulation to dataframe columns
#'
#' Wrapper that applies \code{decumulate_series} to multiple columns within a 
#' dataframe. It automatically detects the date column.
#' 
#' @param df A dataframe containing at least one Date column and cumulative numeric columns.
#' @param col_names Character vector. Names of the columns to decumulate.
#' 
#' @return A dataframe with the specified columns transformed into flows.
#' 
#' @export
decumulate_data <- function(df, col_names) {
  # Find the first non-numeric column (assumed to be Date)
  date_col_idx <- which(!sapply(df, is.numeric))[1]
  dates <- df[[date_col_idx]]
  
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

#' Decumulate all sheets for a country
#'
#' Wrapper to process both Revenues and Expenditures sheets 
#' for a specific country in one call.
#' 
#' @param country_data A list containing two dataframes: \code{Revenues} and \code{Expenditures}.
#' @param rev_cols Character vector. Column names to decumulate in the Revenues sheet.
#' @param exp_cols Character vector. Column names to decumulate in the Expenditures sheet.
#' 
#' @return A list with the same structure as \code{country_data}, but with decumulated flows.
#' 
#' @export
decumulate_country <- function(country_data,
                               rev_cols = "Cash revenues",
                               exp_cols = "Cash expenditures") {
  
  cat("Processing Revenues:\n")
  rev_flow <- decumulate_data(country_data$Revenues, col_names = rev_cols)
  
  cat("Processing Expenditures:\n")
  exp_flow <- decumulate_data(country_data$Expenditures, col_names = exp_cols)
  
  list(
    Revenues     = rev_flow,
    Expenditures = exp_flow
  )
}