build_name <- function(name_indicator, country_name){
  # =========================================================================
  # DESCRIPTION
  # Function to build EA-MD variable names using the indicator name and the
  # country code.
  # -------------------------------------------------------------------------
  # INPUTS
  #     name_indicator : base name of the EA-MD indicator.
  #     country_name   : country name in lower case or mixed case.
  #
  # OUTPUT
  #     name_variable  : EA-MD variable name with the country suffix.
  #
  # -------------------------------------------------------------------------
  
  country_to_code <- c(
    germany = "DE",
    spain   = "ES",
    france  = "FR",
    italy   = "IT",
    ea      = "EA"
  )
  
  country_name <- tolower(country_name)
  code <- country_to_code[[country_name]]
  
  if(is.null(code)){
    stop(paste0("Country ", country_name, " is not implemented"))
  }
  
  name_variable <- paste(name_indicator, "_", code, sep = "")
  return(name_variable)
}



statio_ea_md <- function(list_country, freq = "M"){
  # =========================================================================
  # DESCRIPTION
  # Function to keep EA-MD indicators at a selected frequency and apply the
  # recommended stationarization treatment.
  # -------------------------------------------------------------------------
  # INPUTS
  #     list_country : list containing the EA-MD data and info sheets for one country.
  #     freq         : frequency of the indicators to keep. Either "M" or "Q".
  #
  # OUTPUT
  #     df_data      : dataframe containing stationarized EA-MD indicators.
  #
  # -------------------------------------------------------------------------
  
  if(!freq %in% c("M", "Q")){
    stop(paste0("Frequency ", freq, " is not implemented"))
  }
  
  df_data <- list_country[[1]]
  df_info <- list_country[[2]]
  
  name_idx <- get_col_index(df_info, "Name")
  freq_idx <- get_col_index(df_info, "Frequency")
  tr_idx   <- get_col_index(df_info, "TR1")
  
  df_info <- df_info[df_info[[freq_idx]] == freq, ]
  
  freq_variables <- df_info[[name_idx]]
  df_data <- df_data[, c("Time", freq_variables)]
  
  for(i in 2:ncol(df_data)){
    transfo <- df_info[[tr_idx]][i - 1]
    df_data[, i] <- stationarization_proc(df_data[[i]], transfo)
  }
  
  if(3 %in% df_info[[tr_idx]] | 5 %in% df_info[[tr_idx]]){
    df_data <- df_data[3:nrow(df_data), ]
  }else if(2 %in% df_info[[tr_idx]] | 4 %in% df_info[[tr_idx]]){
    df_data <- df_data[2:nrow(df_data), ]
  }
  
  return(df_data)
}

factors_ea_md <- function(list_country, freq = "M", country_name){
  # =========================================================================
  # DESCRIPTION
  # Function to construct EA-MD factors by group of macro-financial variables.
  # For each group, the first principal component is extracted and used as a
  # synthetic regressor.
  # -------------------------------------------------------------------------
  # INPUTS
  #     list_country : list containing EA-MD data and info sheets for one country.
  #     freq         : frequency of the indicators used to build factors.
  #     country_name : country name used to retrieve the correct EA-MD columns.
  #
  # OUTPUT
  #     df_factors   : dataframe containing dates and group-level factors.
  #
  # -------------------------------------------------------------------------
  
  df_statio <- statio_ea_md(list_country, freq)
  
  df_statio <- zoo::na.locf(df_statio, na.rm = FALSE, fromLast = FALSE)
  df_statio <- zoo::na.locf(df_statio, na.rm = FALSE, fromLast = TRUE)
  
  if(freq == "Q"){
    df_statio <- df_statio[!is.na(df_statio[[2]]), ]
  }
  
  labor_market <- get_group_columns(build_name("UNETOT", country_name),
                                    build_name("UNEU25", country_name),
                                    df_statio)
  
  # some financial variables for the euro area have the suffix "EACC", not simply EA
  if (tolower(country_name) == "ea") {
    
    financial <- c(
      "REER42_EA",
      "ERUS_EA",
      "IRT3M_EACC",
      "IRT6M_EACC",
      "LTIRT_EACC",
      "CURR_EACC",
      "M1_EACC",
      "M2_EACC",
      "SHIX_EA",
      "CAREG_EA"
    )
    
    financial <- financial[financial %in% colnames(df_statio)]
    
    if (length(financial) < 2) {
      stop("Not enough euro-area financial variables found in df_statio.")
    }
    
  } else {
    
    financial <- get_group_columns(
      build_name("SHIX", country_name),
      build_name("LTIRT", country_name),
      df_statio
    )
  }
  
  industrial_prod <- get_group_columns(build_name("IPMN", country_name),
                                       build_name("IPNRG", country_name),
                                       df_statio)
  
  turnover <- get_group_columns(build_name("TRNMN", country_name),
                                build_name("TRNNRG", country_name),
                                df_statio)
  
  prices <- get_group_columns(build_name("PPICAG", country_name),
                              build_name("HICPNG", country_name),
                              df_statio)
  
  confidence <- get_group_columns(build_name("ICONFIX", country_name),
                                  build_name("CCI", country_name),
                                  df_statio)
  
  if(freq == "Q"){
    nat_accounts <- get_group_columns(build_name("GDP", country_name),
                                      build_name("GHSR", country_name),
                                      df_statio)
    
    labor_market <- get_group_columns(build_name("TEMP", country_name),
                                      build_name("ESC", country_name),
                                      df_statio)
    
    credit_aggregates <- get_group_columns(build_name("TASS.SDB", country_name),
                                           build_name("HHLB.LLN", country_name),
                                           df_statio)
    
    labor_costs <- get_group_columns(build_name("ULCIN", country_name),
                                     build_name("ULCPR", country_name),
                                     df_statio)
    
    prices <- get_group_columns(build_name("PPICAG", country_name),
                                build_name("HPRC", country_name),
                                df_statio)
  }
  
  if(freq == "Q"){
    list_groups <- list(
      Nat_indicators = nat_accounts,
      Labor          = labor_market,
      Credit         = credit_aggregates,
      Labor_costs    = labor_costs,
      Financials     = financial,
      IndustrialProd = industrial_prod,
      Turnover       = turnover,
      Prices         = prices,
      Confidence     = confidence
    )
  }else{
    list_groups <- list(
      IndustrialProd = industrial_prod,
      Labor          = labor_market,
      Financials     = financial,
      Turnover       = turnover,
      Prices         = prices,
      Confidence     = confidence
    )
  }
  
  df_factors <- data.frame(matrix(nrow = nrow(df_statio),
                                  ncol = length(list_groups) + 1))
  colnames(df_factors) <- c("Date", names(list_groups))
  df_factors[, 1] <- df_statio[, 1]
  
  for(i in 1:length(list_groups)){
    group <- list_groups[[i]]
    df_group <- df_statio[, group]
    
    factor <- factor_estimation(df_group,
                                kmax = ncol(df_group),
                                fixed = TRUE)
    
    df_factors[[i + 1]] <- factor
  }
  
  return(df_factors)
}


factors_list_ea_md <- function(global_list, freq = "M"){
  # =========================================================================
  # DESCRIPTION
  # Function to compute EA-MD factors for all countries contained in a global
  # list of country-level EA-MD datasets.
  # -------------------------------------------------------------------------
  # INPUTS
  #     global_list  : named list containing EA-MD data/info lists by country.
  #     freq         : frequency used to build the factors.
  #
  # OUTPUT
  #     list_factors : named list containing factor dataframes by country.
  #
  # -------------------------------------------------------------------------
  
  list_factors <- list()
  
  for(i in 1:length(global_list)){
    country_name <- names(global_list)[i]
    list_country <- global_list[[country_name]]
    
    df_factors <- factors_ea_md(list_country,
                                freq = freq,
                                country_name = country_name)
    
    list_factors[[country_name]] <- df_factors
  }
  
  return(list_factors)
}