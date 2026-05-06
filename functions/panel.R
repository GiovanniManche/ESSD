# Function to bridge several dataset with a given frequency for
# numerous country in a panel format 
# Input: 
# - list_df: list of dataframes for each country
# - list_country_name: list of strings to identify each country
# - start_date and end_date: parameters to determine the window over which we want to keep data

# Output:
# - panel_df: panel dataframe

build_panel <- function(list_df, list_country_name, start_date, end_date){
  if(length(list_df) != length(list_country_name)){
    stop("Both list must have the same number of elements")
  }
  
  #  We retrieve the first dataframe from which we build the panel
  df_panel <- list_df[[1]]
  name_df_panel <- list_country_name[[1]]
  
  # We build a column with the name of the country and add it first
  col_name <- rep(name_df_panel, nrow(df_panel))
  df_panel <- cbind("Country" = col_name, df_panel)
  
  # We loop over all remaining dataframes
  for(i in 2:length(list_df)){
    df_country <- list_df[[i]]
    name_country <- list_country_name[[i]]
    col_name <- rep(name_country, nrow(df_country))
    df_country <- cbind("Country" = col_name, df_country)
    
    # We make a check on the columns
    if(identical((df_country),colnames(df_panel))){
      stop("Both dataframes must have the same columns")
    }
    
    # We aggregate (no check on the number of rows to allow for unbalanced panels)
    df_panel <- rbind(df_panel, df_country)
  }
  
  # We sort the panel by time
  df_panel <- df_panel[order(df_panel[[2]]), ]
  
  # We only keep values between start and end dates
  df_panel <- df_panel[(df_panel[[2]]>= start_date) & (df_panel[[2]]<=end_date), ]
  return(df_panel)
}