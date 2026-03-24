# Block of functions used to download data from dbnomics

# Function to import a serie from dbnomics and keep the relevant values
# @id_serie: id of the serie to retrieve over dbnomics
# @df_serie: dataframe containing the period and the values for the given serie
# over the period
if(!require("stringr"))install.packages("stringr");library(stringr)

import_db <- function(id_serie){
  tryCatch(df_serie <- rdb(id_serie),
           error = "Error with data import from dbnomics, verify that the 
           id used for the serie is correct", id_serie)
  
  # We only keep the relevant features
  df_serie_clean <- df_serie[,c("period", "value")]
  colnames(df_serie_clean)[1] <- "Dates"
  return(df_serie_clean)
}

# Function to build a DBnomics ID usable for a given country / period
# Word of caution: this function is built to be used to import OECD revisions dataset
# It may not generalize to other dbnomics series
build_id <- function(serie, country, period, frequency){
  # We get the ID for the particular country for which we want to extract data
  country_id <- switch(country,
         "Germany" = "DEU",
         "Italy" = "ITA",
         "France" = "FRA",
         "Spain" = "ESP",
         "Not implemented for the given country")
  
  # We build the date key
  current_month <- month(period)
  current_year <- year(period)
  if(current_month < 10){
    period_str <- paste(as.character(current_year),"0",as.character(current_month),
                        sep="")
  }else{
    period_str <- paste(as.character(current_year), as.character(current_month),
                        sep="")
  }
  
  # We find the index of the frequency (to insert the name of the country)
  if(frequency == "M"){
    freq_id <- "M"
  }else if(frequency == "Q"){
    freq_id <- "Q"
  }else{
    stop("Frequencies other than monthly and quarterly are not implemented")
  }
  
  if(freq_id == "M"){
    id_db <- str_replace(serie, fixed(".M"), paste(country_id,".M", sep=""))
  }else{
    id_db <- str_replace(serie, fixed(".Q"), paste(country_id,".Q",sep=""))
  }
  
  # We concatenate with the periodicity and retrieve the key
  id_db <- paste(id_db, ".",period_str,sep="")
  return(id_db)
}

# Test of the build_id function
serie <- "OECD/DSD_STES_REVISIONS@DF_STES_REVISIONS/.Q.B1GQ_D.IX._T"
test_id <- build_id(serie, "Germany", test_df_expanding$Dates[9], "Q")
test_import <- import_db(test_id)

# This function builds a mixed-frequency dataframe for a given countries
# based after a vector of series to retrieve from dbnomics
# Only frequencies considered: Monthly and Quarterly
call_db <- function(vect_series, vect_name, country, vect_period, vect_freq, 
                    init_date){
  # We find the most recent observation period and build the dataset to store
  # our result
  last_date <- max(vect_period)
  date_vec <- date_interval_builder(init_date, last_date)
  df_all_data <- data.frame(matrix(nrow = length(date_vec), ncol = length(vect_name)+1))
  df_all_data[,1] <- date_vec
  colnames(df_all_data)<-c("Dates", vect_name)
  
  # We loop over each serie
  for(i in 1:length(vect_series)){
    serie <- vect_series[i]
    freq <- vect_freq[i]
    date <- vect_period[i]
    
    # We build the corresponding dbnomics id key and import the data 
    id_db <- build_id(serie, country, date, freq)
    df_db <- import_db(id_db)
    
    # We retrieve the associated values 
    rows_data <- match(df_db[[1]], as.Date(df_all_data[,1]))
    valid_match <- !is.na(rows_data)
    df_all_data[rows_data[valid_match],i+1] <- df_db[valid_match,2]
  }
  return(df_all_data)
}

vect_serie_test <- c("OECD/DSD_STES_REVISIONS@DF_STES_REVISIONS/.Q.B1GQ_D.IX._T",
                     "OECD/DSD_STES_REVISIONS@DF_STES_REVISIONS/.M.TOVM.IX.G47")
vect_name_test <- c("GDP quarter","Retail trade")
vect_freq_test <- c("Q","M")
vect_period_test <- c(test_df_rolling$Dates[8], test_df_rolling$Dates[9])
init_date <- test_df_expanding$Dates[1]
test_all_imp <- call_db(vect_serie_test, vect_name_test,
                        "Germany", vect_period_test, vect_freq_test, init_date)

# NOT TESTED
# Function to build a vintage dataset using combining dbnomics and Excel data
# Inputs :
# - df_sources: dataframe mentioning for each serie the source of the data
# - country: string containing the country for which we retrieve data
# - vect_period: vector containing the last available data point for each serie
# - date_init: first date for which we want to retrieve data

build_vintage_data <- function(df_sources,country, vect_period,
                               date_init){
  # Building of the final dataset to store results 
  last_date <- max(vect_period)
  date_vec <- date_interval_builder(init_date, last_date)
  ### Attention au format de df_sources - a debug quand sera prêt
  df_all_data <- data.frame(matrix(nrow = length(date_vec), ncol = ncol(df_sources)+1))
  df_all_data[,1] <- date_vec
  colnames(df_all_data)<-c("Dates", colnames(df_sources)[2:ncol(df_sources)])
  
  # We build empty vectors to store the data we will have to retrieve from dbnomics
  vect_serie <- c()
  vect_name_db <- c()
  vect_freq <- c()
  
  # We loop over all series 
  for(i in 2:ncol(df_sources)){
    # We check whether the vintage data should be retrieved from dbnomics 
    # =(4th row of the excel different from other) ==> à tester
    if(df_sources[4,i] != "Other"){
      vect_serie <- append(vect_serie, df_sources[4,i])
      vect_name_db <- append(vect_name_db, colnames(df_sources)[i])
      vect_freq <- append(vect_freq, df_sources[2,i])
    }else{
      print("Method must be implemented for Excel stored data")
    }
  }
  
  # We return all required data from dbnomics
  df_db <- call_db(vect_series, vect_name_db, country, vect_period,
                   vect_freq, data_init)
  
  # We put the vintage data retrieved from dbnomics in the main dataframe
  df_all_data[, vect_name_db] <- db_db[, vect_name_db]
  return(df_all_data)  
}

