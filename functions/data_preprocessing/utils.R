# Files containing useful function for transformation applied to dataframes
# and other data treatments

# Function to choose between a rolling and expanding scheme to handle ragged edge dataset
available_data <- function(df, n, target_date, method = "rolling"){
  if(method == "rolling"){
    df_available_data <- rolling_available_data(df, n)
  }else if(method == "expanding"){
    df_available_data <- expanding_available_data(df,target_date)
  }else{
    stop("Requested method not implemented to get available data")
  }
  return(df_available_data)
}

# Function to retrieve the last n available values for a dataframe containing missing
# observations
# Inputs:
# - df: a dataframe 
# - n: number of last values to retrieve
# Output:
# - df_available_data: df containing last available non null n values for each col

rolling_available_data <- function(df, n){
  # We retrieve the last available n values for each column (null otherwise)
  # as a list
  last_values <- lapply(df, function(x){
    available_data <- x[!is.na(x)]
    if (length(available_data) >= n){
      return(tail(available_data,n))
    }else{
      return(NULL)
    }
  })
  
  # We filter the list to only keep non null values (= column for which we 
  # have observations)
  list_available_data <- Filter(Negate(is.null), last_values)
  
  # We concatenate the vectors of the list to build our matrix
  df_available_data <- as.data.frame(list_available_data)
  # to only keep numerical values (= without date values)
  # df_available_data <- df_available_data[,-1]
  return(df_available_data)
}

# Functions to retrieve all past available values (per month) until a given date
# after taking into account the publication delay
# Inputs :
# - df_ragged: dataframe after taking into account the publication delay
# - date_end: date up to which we want to retrieve values
# Output : 
# - df_expanding_val: dataframe containing the values available up to a given point in time
expanding_available_data <- function(df_ragged, date_end){
  # Sanity checks
  if(date_end <= df_ragged[1,1]){
    stop("The terminal date must be at least superior to first date available")
  }else if(date_end > df_ragged[nrow(df_ragged),1]){
    stop("The terminal date must be inferior or equal to last date available")
  }
  
  # We build a vector of monthly data from first date available to the point for
  # which the expanding window is over
  first_date <- df_ragged[1,1]
  vect_date <- date_interval_builder(first_date, date_end)
  
  # We build the dataframe in which we will store our results
  df_expanding_val<-data.frame(matrix(nrow = length(vect_date), 
                                      ncol = ncol(df_ragged)))
  colnames(df_expanding_val) <- colnames(df_ragged)
  df_expanding_val[,1] <- vect_date
  
  # We only keep values for the ragged dataset up to date_end
  df_ragged_available <- subset(df_ragged, df_ragged[,1]<=date_end) 
  
  # We loop over all publication date available up to this period
  for(i in 1:nrow(df_ragged_available)){
    # We retrieve the publication date and modify it
    current_date <- df_ragged_available[i,1]
    date_monthly <- date_modifyer(current_date, days = 1)
    
    # We find the associated row in our final dataset
    row_date <- which(df_expanding_val[,1]==date_monthly)
    # Case where the date wouldn't be in the new dataset, we skip
    if(length(row_date)==0){next}
    
    # We loop over each variable to retrieve any available value during this month
    for(j in 2:ncol(df_ragged_available)){
      if(is.na(df_ragged_available[i,j])==FALSE){
        df_expanding_val[row_date, j] <- df_ragged_available[i,j]
      }
    }
  }
  
  # We suppress the date column (can be used for debug) and return the dataframe
  # df_expanding_val <- df_expanding_val[,-1]
  return(df_expanding_val)
}

# Function to modify a date arbitrarily with named argument day, month, year
date_modifyer <- function(date, days=0, month=0, year=0){
  # Modification of the date
  day(date) <- days
  
  # Adding / Substracting months or years
  date <- date %m+% months(month)
  date <- date%m+% years(year)
  return(date)
}

# Function to retrieve the last day (Sunday, Monday, Tuesday, ...) for a given date
# (useful because some of the date are published "on the first or last thursday of current month
# such as consumer confidence)
get_thursday <- function(date, day = 5, position = "last") {
  
  # first date of the month
  first_date <- floor_date(date, "month")
  print(first_date)
  # Case where we want to retrieve the first thursday of the month
  if(position == "first"){
    # Number of days until next thursday
    diff <- (wday(first_date)+5) %% 7
    return(first_date + days(diff))
    
  }else if(position == "last"){
    # Last day of the month for our date
    last_day <- rollback(date + months(1))
    
    # Number of days since last thursday
    diff <- (wday(last_day) - 5) %% 7
    return(last_day - days(diff))
  }else{
    stop("Position must be either first or last")
  }
}

# Function to build a monthly vector of date between an init and a terminal date
# The dates considered are arbitrarily of the following form: 01/mm/yyyy
date_interval_builder <- function(date_init, date_terminal){
  if(date_init > date_terminal){
    stop("Initial date has to be below the terminal date")
  }
  days <- 1
  # Vector to store all months
  current_date <- date_modifyer(date_init, days=days)
  vect_date <- c()
  # Loop to get all dates between current and terminal for chosen format
  while(current_date<=date_terminal){
    vect_date <- append(vect_date, current_date)
    current_date <- date_modifyer(current_date, days = days, month = 1)
  }
  return(vect_date)
}

# Function to transform a high-frequency serie to a lower-frequency for a given 
# aggregating operation
# Inputs:
# - data: dataframe containing the data we want to aggregate
# - current_date: date for which we need an aggregated data
# - freq: freq for which we want the serie 
# - op: type of operation to be performed (only mean implemented for this project)
# Outputs:
# - the aggregated value to associate with the current date

rolling_window_aggregator <- function(data, current_date, freq, op = "mean"){
  
  # We determine the number of periods to shift for our aggregator
  if(freq == "month"){
    months <- -1
    years <- 0
  }else if(freq == "year"){
    months <- 0
    years <- -1
  }else{
    print(freq)
    stop("Function not implemented for this frequency")
  }
  
  # Ensure date column is Date
  data[[1]] <- as.Date(data[[1]])
  
  # We get the previous relevant date to compute our aggregated serie
  prec_date <- date_modifyer(current_date, month = months, year = years)
  
  # We filter the dataframe to keep all the available value between both periods
  # for this function to work: date column must come first in the dataframe
  data <- data[data[,1]>prec_date & data[,1]<=current_date, ]
  
  # We compute the aggregated value associated with the current period
  if(op == "mean"){
    agg_value <- mean(data[,2], na.rm = TRUE)
  }else{
    stop("Other operations not implemented")
  }
  
  return(agg_value)
}

# Function to loop over all relevant date from a low frequency dataframe
# in order to retrieve the associated aggregated feature from a high-frequency dataframe
# Inputs:
# -data_target: dataframe containing the date and feature with a low frequency
# -data_feature: dataframe containing the feature at high frequency we must aggregate
# -freq: the frequency of the series in data_target
# Output:
# -data_target with an additional columns corresponding to aggregated variable

time_series_aggregator <- function(data_target, data_feature, freq){
  # For this function to work propery, data_feature must be a (n,2) dataframe
  # with first column containing dates and the second one containing the feature to aggregate
  data[[colnames(data_feature)[2]]] <- NA 
  
  
  # We ensure that the first column of data_target is indeed in date
  data_target[[1]] <- as.Date(data_target[[1]])
  
  # we loop over all dates of data_target to get the aggregated feature
  for(t in 1:nrow(data_target)){
    current_date <- data_target[t,1]
    data_target[t,ncol(data_target)] <-  rolling_window_aggregator(data_feature, current_date, freq)
  }
  return(data_target)
}

# Small function to get the column index of a dataframe
get_col_index <- function(data, col_name){
  idx <- which(colnames(data)==col_name)
  
  if(is.na(idx)){
    stop(paste0("Column '", col_name, "' not found in the dataframe"))
  }
  
  return(idx)
}