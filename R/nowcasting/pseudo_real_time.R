# ==============================================================================
# PSEUDO REAL TIME FRAMEWORK
# ==============================================================================
# In order to test our nowcast models, we need to "predict" the past known values
# of our target variables, with the information available at that time. This is 
# exactly what the functions in this file aim to do, and is absolutely central
# to our exercise. It handles the information flow gestion and starts model estimations
# each time a new information was published but only with the data up to that date,
# thus simulating the past situation.


pseudo_real_time <- function(df_ragged,
                             start_date,
                             end_date = NULL,
                             method = "expanding",
                             model = "factor_midas",
                             n_window = 36,
                             
                             # Factor-MIDAS parameters
                             kmax = 3,
                             lags_factors = 2,
                             
                             # sg-LASSO-MIDAS parameters
                             x_lags = 2,
                             y_lags = 1,
                             legendre_degree = 0,
                             gamma = 0.5,
                             K = NULL,
                             l = 2,
                             penalize_y_lags = FALSE,
                             
                             verbose = TRUE) {
  
  # =========================================================================
  # DESCRIPTION
  # Function to run pseudo-real-time nowcasting.
  # At each publication date, the function builds the vintage dataset available
  # at that date, estimates the chosen nowcasting model, and stores the nowcast.
  #
  # Two models are available:
  #   - "factor_midas": PCA factors + MIDAS regression
  #   - "sg_lasso": sparse-group LASSO MIDAS regression
  # -------------------------------------------------------------------------
  # INPUTS
  #     df_ragged: ragged-edge dataframe. Dates should be in the first column,
  #                target variable in the second column, predictors after.
  #     start_date: first date at which nowcasts are computed.
  #     end_date: last date at which nowcasts are computed. If NULL, all dates
  #               after start_date are used.
  #     method: "expanding" or rolling-window method used by available_data().
  #     model: model used for nowcasting. Either "factor_midas" or "sg_lasso".
  #     n_window: number of observations kept if method is not "expanding".
  #
  #     kmax: maximal number of factors for factor_midas.
  #     lags_factors: number of lags used in the MIDAS regression for factors.
  #
  #     x_lags: number of monthly lags for each predictor in sg_lasso.
  #     y_lags: number of quarterly target lags in sg_lasso.
  #     legendre_degree: degree of the Legendre polynomial basis.
  #     gamma: sparse-group LASSO mixing parameter.
  #     K: number of folds for time-series cross-validation.
  #     l: validation block length in tscv.sglfit.
  #     penalize_y_lags: logical. If FALSE, y lags are not penalized.
  #     verbose: logical. If TRUE, print progress.
  #
  # OUTPUTS
  #     nowcast_results: dataframe containing one row per successful nowcast.
  # -------------------------------------------------------------------------
  
  # ---- STEP 1: basic checks and initialisation
  # Check model argument 
  model <- match.arg(model, choices = c("factor_midas", "sg_lasso", "ar1", "rw"))
  
  # Get dates for which we should nowcast 
  all_dates <- as.Date(df_ragged[[1]])
  start_dt  <- as.Date(start_date)
  
  if (is.null(end_date)) {
    end_dt <- max(all_dates, na.rm = TRUE)
  } else {
    end_dt <- as.Date(end_date)
  }
  
  dates_nowcast <- all_dates[
    all_dates >= start_dt &
      all_dates <= end_dt
  ]
  
  # Initialize output dataframe
  # Common columns are always present.
  # Model-specific columns are filled with NA when not relevant.
  nowcast_results <- data.frame(
    publication_date  = character(),
    model             = character(),
    nowcast_value     = numeric(),
    
    # Factor-MIDAS outputs
    nb_factors        = numeric(),
    
    # sg-LASSO-MIDAS outputs
    lambda            = numeric(),
    gamma             = numeric(),
    cv_mse            = numeric(),
    df                = numeric(),
    n_selected        = numeric(),
    n_selected_groups = numeric(),
    date_new          = as.Date(character()),
    last_obs_y        = numeric(),
    target_name       = character(),
    selected_columns  = character(),
    
    status            = character(),
    error             = character(),
    stringsAsFactors  = FALSE
  )
  
  if (verbose) {
    cat("Nowcasting starts with model:", model, "\n")
  }
  
  # ---- STEP 2: Pseudo-real-time loop ----
  # Each time new information is published, we reestimate the model
  # in order to get a new prediction based using the set of information 
  # augmented with this new one. 
  
  for (i in seq_along(dates_nowcast)) {
    
    current_date <- dates_nowcast[i]
    
    # Get vintage data for the given date.
    n_val <- if (method == "expanding") nrow(df_ragged) else n_window
    
    vintage_data <- available_data(
      df_ragged,
      n = n_val,
      target_date = current_date,
      method = method
    )
    
    # Model selection and estimation 
    if (model == "factor_midas") {
      model_t <- try(
        factor_midas(
          vintage_data,
          kmax = kmax,
          lags_factors = lags_factors
        ),
        silent = TRUE
      )
      
    } else if (model == "sg_lasso") {
      model_t <- try(
        sg_lasso_midas(
          df_input = vintage_data,
          x_lags = x_lags,
          y_lags = y_lags,
          legendre_degree = legendre_degree,
          gamma = gamma,
          K = K,
          l = l,
          penalize_y_lags = penalize_y_lags
        ),
        silent = TRUE
      )
    } else if (model == "ar1") {
      model_t <- try(
        ar1_model(
          df_input = vintage_data
        ),
        silent = TRUE
      )
    } else if (model == "rw") {
      model_t <- try(
        rw_model(
          df_input = vintage_data
        ),
        silent = TRUE
      )
    }
    
    # ---- STEP 3: Save results ----
    # Store results if estimation succeeded
    if (!inherits(model_t, "try-error")) {
      if (model == "factor_midas") {
        new_results <- data.frame(
          publication_date  = as.character(current_date),
          model             = model,
          nowcast_value     = as.numeric(model_t$nowcast),
          
          nb_factors        = model_t$nb_factors,
          
          lambda            = NA_real_,
          gamma             = NA_real_,
          cv_mse            = NA_real_,
          df                = NA_real_,
          n_selected        = NA_real_,
          n_selected_groups = NA_real_,
          date_new          = as.Date(model_t$date_new),
          last_obs_y        = as.numeric(model_t$last_obs_y),
          target_name       = colnames(vintage_data)[2],
          selected_columns  = NA_character_,
          
          status            = "OK",
          error             = NA_character_,
          stringsAsFactors  = FALSE
        )
        
        if (verbose) {
          cat("[OK] Date:", as.character(current_date),
              "Model:", model,
              "Nb factors:", model_t$nb_factors,
              "Nowcast:", round(as.numeric(model_t$nowcast), 4), "\n")
        }
        
      } else if (model == "sg_lasso") {
        
        selected_cols <- if (nrow(model_t$selected_variables) > 0) {
          paste(model_t$selected_variables$column, collapse = "; ")
        } else {
          NA_character_
        }
        
        new_results <- data.frame(
          publication_date  = as.character(current_date),
          model             = model,
          nowcast_value     = as.numeric(model_t$nowcast),
          
          nb_factors        = NA_real_,
          
          lambda            = as.numeric(model_t$lambda),
          gamma             = as.numeric(model_t$gamma),
          cv_mse            = as.numeric(model_t$cv_mse),
          df                = as.numeric(model_t$df),
          n_selected        = as.numeric(model_t$n_selected),
          n_selected_groups = as.numeric(model_t$n_selected_groups),
          date_new          = as.Date(model_t$date_new),
          last_obs_y        = as.numeric(model_t$last_obs_y),
          target_name       = as.character(model_t$target_name),
          selected_columns  = selected_cols,
          
          status            = "OK",
          error             = NA_character_,
          stringsAsFactors  = FALSE
        )
        
        if (verbose) {
          cat("[OK] Date:", as.character(current_date),
              "Model:", model,
              "Lambda:", round(as.numeric(model_t$lambda), 6),
              "Selected:", model_t$n_selected,
              "Nowcast:", round(as.numeric(model_t$nowcast), 4), "\n")
        }
      } else if (model %in% c("ar1", "rw")) {
        new_results <- data.frame(
          publication_date  = as.character(current_date),
          model             = model,
          nowcast_value     = as.numeric(model_t$nowcast),
          
          nb_factors        = NA_real_,
          lambda            = NA_real_,
          gamma             = NA_real_,
          cv_mse            = NA_real_,
          df                = NA_real_,
          n_selected        = NA_real_,
          n_selected_groups = NA_real_,
          date_new          = as.Date(model_t$date_new),
          last_obs_y        = as.numeric(model_t$last_obs_y),
          target_name       = colnames(vintage_data)[2],
          selected_columns  = NA_character_,
          
          status            = "OK",
          error             = NA_character_,
          stringsAsFactors  = FALSE
        )
        
        if (verbose) {
          cat("[OK] Date:", as.character(current_date),
              "Model:", model,
              "Nowcast:", round(as.numeric(model_t$nowcast), 4), "\n")
        }
      }
      
      nowcast_results <- rbind(nowcast_results, new_results)
      
    } else {
      
      # Store failed estimation 
      err_msg <- attr(model_t, "condition")$message
      
      new_results <- data.frame(
        publication_date  = as.character(current_date),
        model             = model,
        nowcast_value     = NA_real_,
        
        nb_factors        = NA_real_,
        
        lambda            = NA_real_,
        gamma             = ifelse(model == "sg_lasso", gamma, NA_real_),
        cv_mse            = NA_real_,
        df                = NA_real_,
        n_selected        = NA_real_,
        n_selected_groups = NA_real_,
        date_new          = as.Date(NA),
        last_obs_y        = NA_real_,
        target_name       = colnames(vintage_data)[2],
        selected_columns  = NA_character_,
        
        status            = "KO",
        error             = err_msg,
        stringsAsFactors  = FALSE
      )
      
      nowcast_results <- rbind(nowcast_results, new_results)
      
      if (verbose) {
        cat("[KO] Date:", as.character(current_date),
            "Model:", model,
            "Reason:", err_msg, "\n")
      }
    }
  }
  
  return(nowcast_results)
}