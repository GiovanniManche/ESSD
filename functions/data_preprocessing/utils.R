# Files containing useful function for transformation applied to dataframes
# and other data treatments

# Function to retrieve the last n available values for a dataframe containing missing
# observations
# Inputs:
# - df: a dataframe 
# - n: number of last values to retrieve
# Output:
# - mat_non_null: matrix containing last available non null n values for each col

available_data <- function(df, n){
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
    mat_available_data <- do.call(cbind,list_available_data)
    return(mat_available_data)
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
