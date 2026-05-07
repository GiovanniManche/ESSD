compute_metrics <- function(actual, predicted) {
  # =========================================================================
  # DESCRIPTION
  # This function computes nowcasting performance metrics and compares models.
  # It includes RMSE, MAE and R2 computations
  # -------------------------------------------------------------------------
  # INPUTS
  #         actual      = numeric vector of realised target values
  #         predicted   = numeric vector of predicted values (nowcasts)
  #          
  # OUTPUTS
  #         list        = list containing the metrics 
  # -------------------------------------------------------------------------
  
  # Coerce to plain numeric vectors
  a <- as.numeric(actual)
  p <- as.numeric(predicted)
  
  # Truncate to shortest length
  min_len <- min(length(a), length(p))
  a <- a[1:min_len]
  p <- p[1:min_len]
  
  # Remove NAs
  valid <- complete.cases(a, p)
  a <- a[valid]
  p <- p[valid]
  n <- length(a)
  
  if (n < 2) {
    return(list(RMSE = NA, MAE = NA, R2 = NA, n = n))
  }
  
  errors <- a - p
  ss_res <- sum(errors^2)
  ss_tot <- sum((a - mean(a))^2)
  
  list(
    RMSE = sqrt(ss_res / n),
    MAE  = mean(abs(errors)),
    R2   = ifelse(ss_tot > 0, 1 - ss_res / ss_tot, NA),
    n    = n
  )
}


metrics_table <- function(results_list) {
  # =========================================================================
  # DESCRIPTION
  # This function builds a metrics comparison table for a particular combination 
  #                                                      model x block x country
  # -------------------------------------------------------------------------
  # INPUTS
  #         results_list    = named list of model (nowcast) results
  #          
  # OUTPUTS
  #         dataframe       =    Data frame with columns: Model, RMSE, MAE, R2, n
  # -------------------------------------------------------------------------
  rows <- lapply(names(results_list), function(model_name) {
    res <- results_list[[model_name]]
    m <- compute_metrics(res$y_target, res$fitted)
    data.frame(
      Model = model_name,
      RMSE  = round(m$RMSE, 4),
      MAE   = round(m$MAE, 4),
      R2    = round(m$R2, 4),
      n     = m$n,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
