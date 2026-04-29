# Function to compute Monetary Policy Surprises à la GSS(2005)
# Input:
# -data_assets: Dataset from the Euro Area containing high frequency response of asset prices
# to ECB meeting (EA-MPD database)
# Output:
# - data_mps: Dataset containing the MPS for each period

mps_builder <- function(data_assets){
  # We find the required indexes to compute the MPS
  date_index <- get_col_index(data_assets, "Date")
  ois_1m_index <- get_col_index(data_assets, "OIS_1M")
  ois_1y_index <- get_col_index(data_assets, "OIS_1Y")
  
  # We convert the date column to date (characters format)
  data_assets$date <- as.Date(as.numeric(data_assets$Date), origin = "1899-12-30")
  
  # We identify the series for which we have NAs and initialize the path factor
  na_idx <- is.na(data_assets[[ois_1m_index]])
  path_factor <- numeric(length(data_assets[[ois_1m_index]]))
  
  # First step: we regress the response of the 1M-OIS future contract on those 
  # of the 1Y-OIS future contract
  reg_mps <- lm(OIS_1M ~ OIS_1Y, data = data_assets)
  
  # We retrieve the residuals (path factor)
  path_factor[!na_idx] <- reg_mps$residuals #### A fixer (faut aussi gérer les NA côté 1y)
  
  # The target factors correspond to the response of the one month OIS future contracts
  target_factor <- data_assets[[ois_1m_index]]
  target_factor[is.na(target_factor)] <- 0
  
  # We normalize both of them and compute the MPS for the Euro Area (sum of both factors)
  path_factor <- path_factor/sd(path_factor, na.rm = TRUE)
  target_factor <- target_factor/sd(target_factor, na.rm = TRUE)
  mps <- path_factor + target_factor
  
  # we build our dataframe of output
  data_mps <- data.frame(matrix(0, ncol = 2, nrow = nrow(data_assets)))
  colnames(data_mps) <- c("Date","MPS")
  data_mps[,1] <- data_assets[,date_index]
  data_mps[,2] <- mps
  return(data_mps)
}

# Test for this function
test <- read_excel("data/EA-EMDP.xlsx", sheet = "emdp")
df_monthly_mps <- mps_builder(test)

# Test for aggregator function
test_agg <- rolling_window_aggregator(df_monthly_mps, as.Date(df_monthly_mps[100,1]), freq = "month")