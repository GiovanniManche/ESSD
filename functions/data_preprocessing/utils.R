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
  if(date_end < df_ragged[1,1]){
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

# Test for the function
df <- data.frame(
  var1 = c(1, 2, NA, 4, 5, 6),  
  var2 = c(10, NA, 20, NA, 30, 40), 
  var3 = c(NA, NA, 100, 200, NA, NA) 
)

# Comparison of the result with expected
mat_function <- available_data(df, n = 3)
mat_expected <- cbind(c(4,5,6), c(20,30,40))
if(sum(isFALSE(mat_function==mat_expected)) == 0){
  print("Test ok for function available_data")
}else{
  error("Error with function available_data")
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

get_thursday(as.Date("2026-03-01"), position = "first") 
get_thursday(as.Date("2026-03-01"), position = "last") 

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
  while(current_date<date_terminal){
    vect_date <- append(vect_date, current_date)
    current_date <- date_modifyer(current_date, days = days, month = 1)
  }
  return(vect_date)
}
test <- date_interval_builder(test_df_ragged[2,1], test_df_ragged[nrow(test_df_ragged),1])
