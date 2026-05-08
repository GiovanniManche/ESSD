# ==========================================================================
# MONETARY POLICY SURPRISES - EURO AREA
# ==========================================================================

mps_builder <- function(data_assets){
  # =========================================================================
  # DESCRIPTION
  # Function to compute Monetary Policy Surprises following Gürkaynak, 
  # Sack and Swanson (2005), adapted to Euro Area data.
  # The surprise is built as the sum of two standardized factors:
  #   - a target factor, captured by the response of the 1-month OIS;
  #   - a path factor, obtained as the residual from a regression of the 
  #     1-month OIS response on the 1-year OIS response.
  # -------------------------------------------------------------------------
  # INPUTS
  #     data_assets : dataframe containing high-frequency asset price responses
  #                   around ECB monetary policy announcements.
  #                   Required columns: Date, OIS_1M, OIS_1Y.
  #
  # OUTPUT
  #     data_mps    : dataframe containing the date and the associated monetary
  #                   policy surprise.
  #
  # -------------------------------------------------------------------------
  
  # Retrieve column indexes
  date_index <- get_col_index(data_assets, "Date")
  ois_1m_index <- get_col_index(data_assets, "OIS_1M")
  ois_1y_index <- get_col_index(data_assets, "OIS_1Y")
  
  # Convert Excel-format dates into standard Date format
  data_assets$date <- as.Date(as.numeric(data_assets$Date), origin = "1899-12-30")
  
  # Identify observations with missing 1-month OIS responses
  na_idx <- is.na(data_assets[[ois_1m_index]])
  
  # Initialize path factor vector
  path_factor <- numeric(length(data_assets[[ois_1m_index]]))
  
  # Regress the response of the 1-month OIS on the response of the 1-year OIS
  # The residual captures the path component of the monetary policy surprise
  reg_mps <- lm(OIS_1Y ~ OIS_1M, data = data_assets)
  
  # Store residuals only for non-missing observations
  path_factor[!na_idx] <- reg_mps$residuals
  
  # The target factor is directly given by the 1-month OIS response
  target_factor <- data_assets[[ois_1m_index]]
  
  # Replace missing target factor values by zero
  target_factor[is.na(target_factor)] <- 0
  
  # Standardize both factors before aggregation
  path_factor <- path_factor / sd(path_factor, na.rm = TRUE)
  target_factor <- target_factor / sd(target_factor, na.rm = TRUE)
  
  # Monetary policy surprise: sum of standardized target and path factors
  mps <- path_factor + target_factor
  
  # Build output dataframe
  data_mps <- data.frame(matrix(0, ncol = 2, nrow = nrow(data_assets)))
  colnames(data_mps) <- c("Date", "MPS")
  data_mps[, 1] <- data_assets[, date_index]
  data_mps[, 2] <- mps
  
  return(data_mps)
}