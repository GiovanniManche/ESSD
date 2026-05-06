# Import of the EA-MD dataset
# A réconcilier avec la main 

# We map country codes to country names
code_to_country <- c(
  DE = "germany",
  ES = "spain",
  FR = "france",
  IT = "italy"
)

files <- list.files(here("data/EA_MD_DB"), pattern = "\\.xlsx$", full.names = TRUE)
for (f in files) {
  base_name <- tools::file_path_sans_ext(basename(f))
  
  # We retrieve the country code (first two letters)
  code <- substr(base_name, 1, 2)
  
  if (code %in% names(code_to_country)) {
    country_name <- code_to_country[[code]]
    obj_name <- paste0("EA_MD_", country_name)
    
    sheets_of_country <- list(
      data     = read_excel(f, sheet = "data"),
      info = read_excel(f, sheet = "info")
    )
    assign(obj_name, sheets_of_country, envir = .GlobalEnv)
  } else {
    next
  }
}

# We retrieve all EA-MD data
countries_ea_md <- list(
  Germany = EA_MD_germany,
  Italy   = EA_MD_italy,
  France  = EA_MD_france,
  Spain   = EA_MD_spain
)


# Function to only keep indicators with the relevant frequency (quarterly ou monthly)
statio_ea_md <- function(list_country, freq = "M"){
  
  # We verify whether freq is in M or Q (error otherwise)
  if(!freq %in% c("M","Q")){
    stop(paste0("Frequency ", freq, " is not implemented"))
  }
  
  # We retrieve the dataframe containing the information / data
  df_data <- list_country[[1]]
  df_info <- list_country[[2]]
  
  # We retrieve the index for the frequency, name and treatment column
  name_idx <- get_col_index(df_info, "Name")
  freq_idx <- get_col_index(df_info, "Frequency")
  tr_idx <- get_col_index(df_info, "TR1")
  
  # We filter the dataframe with information
  df_info <- df_info[df_info[[freq_idx]] == freq, ]
  
  # We only keep the variables associated with the particular frequency
  freq_variables <- df_info[[name_idx]]
  df_data <- df_data[, c("Time",freq_variables)]
  
  # We transform each serie following the recommended treatment
  for(i in 2:ncol(df_data)){
    serie <- as.data.frame(df_data[1:nrow(df_data),i])
    transfo <- df_info[[tr_idx]][i-1]
    # test <- stationarization_proc(serie, transfo)
    df_data[,i] <- stationarization_proc(df_data[[i]], transfo)
  }
  
  # We suppress the first rows if differenciation was performed
  if(3%in%df_info[[tr_idx]] | 5%in%df_info[[tr_idx]]){
    df_data <- df_data[3:nrow(df_data),]
  }else if(2%in%df_info[[tr_idx]] | 4%in%df_info[[tr_idx]])
    df_data <- df_data[2:nrow(df_data),]
  else{
    df_data <- df_data
  }
  return(df_data)
}

# Test pour la fonction de stationarisation
# test_statio_ea <- statio_ea_md(EA_MD_france)

# Fonction pour construire les facteurs par groupe de variables
# Inputs:
# - list_ea_md: list containing the dataframes with EA-MD data and info for a given country
# - freq: frequency for the indicators we plan to use to build the factors
# Outputs:
# - df_factor: dataframe containing the values of the factors for the different 
# group of monthly variables in the dataset

# Intermediary function to retrieve the name of the columns depending on the country
# (linked to the way EA-MD dataset is built)
build_name <- function(name_indicator, country_name){
  
  # Mapping between country name and code
  country_to_code <- c(
    germany = "DE",
    spain = "ES",
    france = "FR",
    italy = "IT"
  )
  
  
  # We impose the country name to be lower case and retrieve the associated code
  country_name <- tolower(country_name)
  code <- country_to_code[[country_name]]

  # We build the variable name
  name_variable <- paste(name_indicator, "_", code, sep="")
  return(name_variable)
}

factors_ea_md <- function(list_country, freq = "M", country_name){
  
  # Stationarize the serie
  df_statio <- statio_ea_md(list_country, freq)
  
  # We remove remaining missing values
  df_statio <- zoo::na.locf(df_statio, na.rm = FALSE, fromLast = FALSE)
  df_statio <- zoo::na.locf(df_statio, na.rm = FALSE, fromLast = TRUE)
  
  # If the frequency wanted is quarterly, we only keep the last value per quarter
  if(freq == "Q"){
    df_statio <- df_statio[!is.na(df_statio[[2]]), ]
  }
  
  # We retrieve all groups of variables available both at monthly and quarterly frequency
  # Group of variables for labor market indicators
  labor_market <- get_group_columns(build_name("UNETOT", country_name), 
                                    build_name("UNEU25", country_name),
                                    df_statio)
  
  # Group of variables for financial markets indicators and interest rates (we don't keep
  # exchange rate for this application)
  financial <- get_group_columns(build_name("SHIX", country_name), 
                                 build_name("LTIRT", country_name),
                                 df_statio)
  
  # Group for industrial production
  industrial_prod <- get_group_columns(build_name("IPMN", country_name), 
                                       build_name("IPNRG", country_name),
                                       df_statio) 
  
  # Group for turnover index
  turnover <- get_group_columns(build_name("TRNMN", country_name), 
                                build_name("TRNNRG", country_name),
                                df_statio) 
    
  # Group of prices indicator
  prices <- get_group_columns(build_name("PPICAG", country_name), 
                              build_name("HICPNG", country_name),
                              df_statio) 
    
  # Group of confidence indicators
  confidence <- get_group_columns(build_name("ICONFIX", country_name), 
                                  build_name("CCI", country_name),
                                  df_statio) 
    
  if(freq == "Q"){ # The case for this project
    
    # Group for national accounts
    nat_accounts <- get_group_columns(build_name("GDP", country_name), 
                                      build_name("GHSR", country_name),
                                      df_statio)
    
    # Group for labor market indicators
    labor_market <- get_group_columns(build_name("TEMP", country_name), 
                                      build_name("ESC", country_name),
                                      df_statio)
    
    # Credit aggregates
    credit_aggregates <- get_group_columns(build_name("TASS.SDB", country_name), 
                                           build_name("HHLB.LLN", country_name),
                                           df_statio)
    
    # Labor costs
    labor_costs <- get_group_columns(build_name("ULCIN", country_name), 
                                     build_name("ULCPR", country_name),
                                     df_statio)
    
    # Prices
    prices <- get_group_columns(build_name("PPICAG", country_name), 
                                build_name("HPRC", country_name),
                                df_statio)
  }
  
  # We build a list containing all these groups of variables
  if(freq=="Q"){
    list_groups <- list(
      Nat_indicators = nat_accounts,
      Labor   = labor_market,
      Credit  = credit_aggregates,
      Labor_costs  = labor_costs,
      Financials = financial, 
      IndustrialProd = industrial_prod, 
      Turnover = turnover, 
      Prices = prices,
      Confidence = confidence)
  }else{
    list_groups <- list(
      IndustrialProd = industrial_prod, 
      Labor   = labor_market,
      Financials = financial, 
      Turnover = turnover, 
      Prices = prices,
      Confidence = confidence)
  }

  # Initialize our dataframe of results
  df_factors <- data.frame(matrix(nrow = nrow(df_statio), ncol = (length(list_groups)+1)))
  colnames(df_factors) <- c("Date", names(list_groups))
  df_factors[,1] <- df_statio[,1]
  
  # Loop over the groups
  for(i in 1:length(list_groups)){
    
    # We keep only the variables associated with the given group
    group <- list_groups[[i]]
    df_group <- df_statio[, group]
    
    # We compute the factors
    factor <- factor_estimation(df_group, kmax = ncol(df_group), fixed = T) # Kmax not necessary because we use only 1 factors 
    
    # We store the value in our dataframe (first column = date)
    df_factors[[i+1]] <- factor
  }
  
  # Retrieve the dataframe
  return(df_factors)
}

# Test de la fonction pour construire les facteurs
# test_factor <-  factors_ea_md(EA_MD_france, country_name = "france")

# Function to build all regressors (factors, mps, ...) for a given list of countries
# Inputs:
# - global_list: list of list containing all data available in our study imported from EA-MD database (data/info and country names)
# Outputs:
# -list_factors: list containing factors extracted for each countries (to be used as explanatory variables in a panel)

factors_list_ea_md <- function(global_list){
  # We initialize the list of results
  list_factors <- list()
  
  # We loop over all the available data
  for(i in 1:length(global_list)){
    # We retrieve the country name and the associated list of data/info
    country_name <- names(global_list)[i]
    list_country <- global_list[[country_name]]
    
    # We compute the factors for this country and store it (we keep our analysis in monthly frequency)
    df_factors <- factors_ea_md(list_country, freq = "M", country_name)
    list_factors[[country_name]] <- df_factors
  }
  return(list_factors)
}


# Test of list_factor function
test_list_factors <- factors_list_ea_md(countries_ea_md)

# We add MPS
mps_data <- read_excel("data/EA-EMDP.xlsx", sheet = "emdp")
df_mps_real <- mps_builder(test) # Warning ?

#### Shouldn't be common to all countries
####  Block to add all common variables (across countries) in one dataframe
# test_mps <- test_list_factors[[1]]
# test_mps <- time_series_aggregator(test_mps, df_mps, freq="M")

# Loop over all countries to add MPS * shock 
for(i in 1:length(test_list_factors)){
  df_factor <- test_list_factors[[i]]
  
  # We simulate MPS * FRAG (considering MPS * rnorm to have smth with variability across countries 
  # --> To replace with true data
  df_mps <- df_mps_real
  df_mps[,2] <- df_mps[,2]
  
  # We aggregate the data and the fragmentation (for now: white noise)
  df_agg <- time_series_aggregator(df_factor, df_mps, freq="M")
  df_agg$frag <- rnorm(nrow(df_agg), mean = 0, sd = 1)
  test_list_factors[[i]] <- df_agg
}

# Test de la panélisation
test_panel <- build_panel(test_list_factors, names(countries_ea_md),
                          start_date = test_list_factors[[1]][["Date"]][50],
                          end_date = test_list_factors[[1]][["Date"]][300])


# Test of LP regression
# Librairies à ajouter dans la main quand on réconciliera
if(!require(dplyr)) install.packages('dplyr'); library(dplyr)
if(!require(fixest)) install.packages('fixest'); library(fixest)

# Horizon for the LPs
max_h <- 36

# We group the panel by countries
test_panel_2 <- test_panel %>%
  arrange(Country, Date) %>%        
  group_by(Country) 

# Local Projections
results <- lapply(0:max_h, function(h) {
  
  df_h <- test_panel %>%
    mutate(
      y_lead = lead(IndustrialProd, h),   
      y_lag  = lag(IndustrialProd, 1),    
      lhs    = y_lead - y_lag       #  specification for the dependent variable
    ) %>%
    ungroup()
  
  # Regression (clustered at the country level)
  # Fixed effects at the time and country level
  feols(
    lhs ~ MPS:frag + Labor + Financials + Turnover + Prices + Confidence|
      Country + Date,
    data = df_h,
    cluster = ~Country
  )
})

# Results for the interaction coefficient
gamma <- sapply(results, function(m) coef(m)["MPS:frag"])

plot(0:max_h, gamma, type = "b",
     xlab = "Horizon", ylab = "Gamma_h",
     main = "Interaction between Monetary policy and fiscal fragmentation")