build_panel <- function(list_df, list_country_name, start_date, end_date){
  # =========================================================================
  # DESCRIPTION
  # Function to build a country-level panel dataframe from a list of country-
  # specific dataframes.
  # Each dataframe is associated with a country name, added as a first column.
  # The function allows for unbalanced panels, since country dataframes do not
  # need to have the same number of observations.
  # -------------------------------------------------------------------------
  # INPUTS
  #     list_df           : list of dataframes, one for each country.
  #     list_country_name : list/vector of country names associated with the 
  #                         dataframes in list_df.
  #     start_date        : first date to keep in the final panel.
  #     end_date          : last date to keep in the final panel.
  #
  # OUTPUT
  #     df_panel          : panel dataframe with a Country column and all country
  #                         observations stacked together.
  #
  # -------------------------------------------------------------------------
  
  # Check that each dataframe is associated with one country name
  if(length(list_df) != length(list_country_name)){
    stop("Both lists must have the same number of elements")
  }
  
  # Retrieve the first dataframe used to initialize the panel
  df_panel <- list_df[[1]]
  name_df_panel <- list_country_name[[1]]
  
  # Add the country identifier as the first column
  col_name <- rep(name_df_panel, nrow(df_panel))
  df_panel <- cbind("Country" = col_name, df_panel)
  
  # Loop over remaining country dataframes if there is more than one country
  if(length(list_df) > 1){
    for(i in 2:length(list_df)){
      
      # Retrieve country-specific dataframe and country name
      df_country <- list_df[[i]]
      name_country <- list_country_name[[i]]
      
      # Add the country identifier as the first column
      col_name <- rep(name_country, nrow(df_country))
      df_country <- cbind("Country" = col_name, df_country)
      
      # Check that all country dataframes have the same columns
      if(!identical(colnames(df_country), colnames(df_panel))){
        stop("All dataframes must have the same columns")
      }
      
      # Stack country observations into the panel
      # No restriction on the number of rows to allow for unbalanced panels
      df_panel <- rbind(df_panel, df_country)
    }
  }
  
  # Sort the panel by date
  df_panel <- df_panel[order(df_panel[[2]]), ]
  
  # Keep only observations within the selected time window
  df_panel <- df_panel[(df_panel[[2]] >= start_date) & (df_panel[[2]] <= end_date), ]
  
  return(df_panel)
}


estimate_panel_lp <- function(panel_df, y_var, shock_var, interaction_var,
                              controls, max_h = 36){
  # =========================================================================
  # DESCRIPTION
  # Function to estimate panel local projections with country and time fixed
  # effects. The coefficient of interest is the interaction between the monetary
  # policy shock and a country-specific state variable.
  # -------------------------------------------------------------------------
  # INPUTS
  #     panel_df         : panel dataframe containing Country, Date, dependent
  #                        variable, shock, interaction variable and controls.
  #     y_var            : name of the response variable.
  #     shock_var        : name of the monetary policy shock variable.
  #     interaction_var  : name of the variable interacted with the shock.
  #     controls         : vector of control variable names.
  #     max_h            : maximal horizon for the local projections.
  #
  # OUTPUT
  #     results          : list of fixest models, one for each horizon.
  #
  # -------------------------------------------------------------------------
  
  results <- lapply(0:max_h, function(h) {
    
    df_h <- panel_df %>%
      arrange(Country, Date) %>%
      group_by(Country) %>%
      mutate(
        y_lead = dplyr::lead(.data[[y_var]], h),
        y_lag  = dplyr::lag(.data[[y_var]], 1),
        lhs    = y_lead - y_lag
      ) %>%
      ungroup()
    
    rhs_terms <- c(
      paste0(shock_var, ":", interaction_var),
      controls
    )
    
    formula_lp <- as.formula(
      paste0("lhs ~ ",
             paste(rhs_terms, collapse = " + "),
             " | Country + Date")
    )
    
    fixest::feols(
      formula_lp,
      data = df_h,
      cluster = ~Country
    )
  })
  
  return(results)
}


extract_lp_coef <- function(results, shock_var, interaction_var){
  # =========================================================================
  # DESCRIPTION
  # Function to extract the interaction coefficient from a list of local
  # projection models.
  # -------------------------------------------------------------------------
  # INPUTS
  #     results         : list of fixest models.
  #     shock_var       : name of the shock variable.
  #     interaction_var : name of the interaction variable.
  #
  # OUTPUT
  #     coef_path       : vector of estimated coefficients across horizons.
  #
  # -------------------------------------------------------------------------
  
  coef_name <- paste0(shock_var, ":", interaction_var)
  coef_path <- sapply(results, function(m) coef(m)[coef_name])
  
  return(coef_path)
}