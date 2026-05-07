# ==============================================================================
# RAGGED-EDGE VINTAGE DATASET CONSTRUCTION
# ==============================================================================
# This file contains helper functions used to construct a pseudo-real-time
# dataset from publication dates.
#
# The objective is to reproduce the information set that would have been
# available at each point in time. Each series is therefore shifted from its
# reference period date to its actual publication date, according to the
# publication calendar (can be found in the data folder).
#
# ==============================================================================

ragged_edge_dataset <- function(df_data, df_publi){
  # =========================================================================
  # DESCRIPTION
  # Function to build a ragged-edge dataset accounting for heterogeneous
  # publication dates across variables.
  # This function shifts each series to its publication date and then combines
  # all shifted series into a common dataset. The resulting dataset contains
  # missing values whenever a variable has not yet been published at a given
  # date.
  # -------------------------------------------------------------------------
  # INPUTS
  #     df_data: dataframe containing the dates, target variable and predictors.
  #              The first column must contain the date index. The remaining
  #              columns contain the economic series.
  #
  #     df_publi: dataframe containing the publication calendar of each series.
  #
  # OUTPUT
  #     df_ragged_edge: dataframe where each value appears only from its
  #                     publication date onward.
  # =========================================================================
  
  # We build an empty list and vector to stock all series post modif and all dates
  vect_dates <- c()
  list_vintages_df <- list()
  
  # We store the initial vector date
  init_vect_date <- df_data[,1]
  
  # We loop over all series and transform the date according to publication calendar
  for(i in 2:ncol(df_data)){
    df_serie <- data.frame(cbind(df_data[,1], df_data[,i]))
    colnames(df_serie) <- c("Dates",colnames(df_data[,i]))
    
    # Modification of the date index of the serie and storage of dates and transformed df
    df_serie_vintage <- publication_date_shifter(df_serie, df_publi)
    # vect_dates <- append(c, rownames(df_serie_vintage))
    vect_dates <- append(vect_dates, df_serie_vintage[,1])
    list_vintages_df[[length(list_vintages_df) + 1]] <- df_serie_vintage
  }
  
  # We only keep unique dates and store them in increasing order
  vect_date_clear <- sort(unique(vect_dates))
  
  # We build our transformed dataset
  df_ragged_edge <- data.frame(matrix(nrow = length(vect_date_clear), ncol 
                                      = ncol(df_data)))
  df_ragged_edge[,1] <- vect_date_clear
  colnames(df_ragged_edge) <- colnames(df_data)
  
  # Now, for each serie, we only keep values at the timing of their publication date
  for(i in 1:length(list_vintages_df)){
    df_vintage <- list_vintages_df[[i]]
    for(j in 1:nrow(df_vintage)){
      publi_date <- df_vintage[j,1]
      row_ragged_df <- which(grepl(publi_date,df_ragged_edge[,1]))
      df_ragged_edge[row_ragged_df, i+1]<-df_vintage[j,2]
    }
  }
  return(df_ragged_edge)
}


publication_date_shifter <- function(df_serie, df_publi){
  # =========================================================================
  # DESCRIPTION
  # Function which takes a dataframe with a particular serie and shifts all date
  # according to when it is published (must distinguish between trimester and quarter)
  # -------------------------------------------------------------------------
  # INPUTS
  #     df_serie: two-column dataframe containing:
  #                 - first column: reference-period dates
  #                 - second column: values of one economic series
  #
  #     df_publi: dataframe containing the publication calendar.
  #
  # OUTPUT
  #     df_serie: same dataframe as input, but with the date column replaced by
  #               the publication dates.
  # =========================================================================
  
  # We retrieve associated caracteristics of the serie
  freq <- df_publi[1,colnames(df_serie)[2]]
  months <- as.numeric(df_publi[2,colnames(df_serie)[2]])
  days <- df_publi[3, colnames(df_serie)[2]]
  
  # We retrieve the associated series of date (= index)
  #date_vector <- rownames(df_serie)
  date_vector <- df_serie[,1]
  class(df_publi[2, colnames(df_serie)[2]])
  # We apply the shifts (= date modification) considering the case where
  # we don't have a precise date (= first/last thursday)
  if(days == "First Thursday"){
    date_vector_publi <- get_thursday(date_vector, position="first")
  }else if(days == "Last Thursday"){
    date_vector_publi <- get_thursday(date_vector, position="last")
  }else{
    days <- as.numeric(days)
    if (days == 0 && months == 0) {
      date_vector_publi <- date_vector 
    } else {
      date_vector_publi <- date_modifyer(date_vector, days = days, month = months)
    }
  }
  # We modify the index of the serie (after taking into account publication delay)
  #rownames(df_serie) <- date_vector_publi
  df_serie[,1] <- date_vector_publi
  # In the case of quarter date, we apply a forward fill (no new data available until
  # next quarter)
  if(freq == "Q"){
    df_serie[,2] <- na.locf(df_serie[,2])
  }
  return(df_serie)
}