# ==========================================================================
# MONETARY POLICY TRANSMISSION AND FISCAL FRAGMENTATION
# ==========================================================================
# This file contains all functions associated with the euro-area monetary policy
# transmission analysis.
# The empirical objective is to test whether euro-area fiscal fragmentation
# amplifies or dampens the effect of ECB monetary policy surprises on industrial
# production.
#
# The functions allow us to:
#   - build euro-area macro-financial datasets using either GDP-weighted country
#     aggregates or direct euro-area EA-MD factors;
#   - estimate static euro-area regressions with Newey-West standard errors;
#   - estimate local projections with an interaction between monetary policy
#     surprises and fiscal fragmentation;
#   - construct conditional IRFs under low, average and high fragmentation;
#   - build marginal effects and coefficient comparison tables.
#
# Main empirical specifications:
#
# Static regression:
#   Delta Y_t = beta MPS_t + gamma(MPS_t x Frag_t) + delta Frag_t + X_t'theta + epsilon_t
#
# Local projections:
#   Y_{t+h} - Y_{t-1} = beta_h MPS_t + gamma_h(MPS_t x Frag_t) + X_t'theta_h + epsilon_{t+h}
#
# Positive MPS values are interpreted as tightening surprises when OIS responses
# are coded positively for increases in rates.
# ==========================================================================


build_ea_regression_data <- function(source_type = c("weighted_4countries", "direct_ea"),
                                     list_factors,
                                     countries_ea_md,
                                     gdp_long,
                                     EA_MD_ea,
                                     df_mps,
                                     ea_frag) {
  # =========================================================================
  # DESCRIPTION
  # Function to build the euro-area regression dataset used in the monetary
  # policy transmission analysis.
  # The function supports two constructions:
  #   1. GDP-weighted aggregates of country-level EA-MD factors for Germany,
  #      France, Italy and Spain.
  #   2. Direct euro-area EA-MD factors.
  # The resulting dataset is then merged with the euro-area fiscal fragmentation
  # index and contains both the level and first difference of industrial
  # production.
  # -------------------------------------------------------------------------
  # INPUTS
  #     source_type: character. Either "weighted_4countries" or "direct_ea".
  #     list_factors: list of country-level EA-MD factor dataframes.
  #     countries_ea_md: named list of country-level EA-MD data/info lists.
  #     gdp_long: dataframe containing GDP weights by country, year and quarter.
  #     EA_MD_ea: list containing direct euro-area EA-MD data and info sheets.
  #     df_mps: dataframe containing Date and MPS monetary policy surprises.
  #     ea_frag: dataframe containing year_month, fragmentation_index and
  #              centered fragmentation frag_ea_c.
  #
  # OUTPUT
  #     ea_reg_df: euro-area regression dataframe containing macro-financial
  #                variables, MPS, fragmentation variables, d_IndustrialProd
  #                and source label.
  # -------------------------------------------------------------------------
  
  source_type <- match.arg(source_type)
  
  if (source_type == "weighted_4countries") {
    
    list_factors_work <- list_factors
    
    for (i in seq_along(list_factors_work)) {
      df_factor <- list_factors_work[[i]]
      
      df_agg <- time_series_aggregator(
        df_factor,
        df_mps,
        freq = "M"
      )
      
      list_factors_work[[i]] <- df_agg
    }
    
    panel_df_ea <- build_panel(
      list_df = list_factors_work,
      list_country_name = names(countries_ea_md),
      start_date = list_factors_work[[1]][["Date"]][50],
      end_date   = list_factors_work[[1]][["Date"]][300]
    )
    
    panel_df_ea <- panel_df_ea %>%
      mutate(
        Date       = as.Date(Date),
        year_month = format(Date, "%Y-%m"),
        year       = as.integer(format(Date, "%Y")),
        quarter    = ceiling(as.integer(format(Date, "%m")) / 3)
      ) %>%
      left_join(
        gdp_long,
        by = c("Country" = "country", "year", "quarter")
      )
    
    ea_df <- panel_df_ea %>%
      group_by(Date, year_month) %>%
      summarize(
        IndustrialProd = weighted.mean(IndustrialProd, w = weight, na.rm = TRUE),
        Labor          = weighted.mean(Labor,          w = weight, na.rm = TRUE),
        Financials     = weighted.mean(Financials,     w = weight, na.rm = TRUE),
        Turnover       = weighted.mean(Turnover,       w = weight, na.rm = TRUE),
        Prices         = weighted.mean(Prices,         w = weight, na.rm = TRUE),
        Confidence     = weighted.mean(Confidence,     w = weight, na.rm = TRUE),
        MPS            = first(na.omit(MPS)),
        .groups        = "drop"
      ) %>%
      arrange(Date)
    
    source_label <- "GDP-weighted country aggregates"
  }
  
  if (source_type == "direct_ea") {
    
    ea_list <- list(
      ea = EA_MD_ea
    )
    
    ea_factors_list <- factors_list_ea_md(
      global_list = ea_list,
      freq = "M"
    )
    
    ea_df <- ea_factors_list[[1]]
    
    ea_df <- time_series_aggregator(
      ea_df,
      df_mps,
      freq = "M"
    )
    
    ea_df <- ea_df %>%
      mutate(
        Date = as.Date(Date),
        year_month = format(Date, "%Y-%m")
      ) %>%
      arrange(Date)
    
    source_label <- "Direct euro-area EA-MD factors"
  }
  
  ea_reg_df <- ea_df %>%
    left_join(ea_frag, by = "year_month") %>%
    arrange(Date) %>%
    mutate(
      d_IndustrialProd = IndustrialProd - lag(IndustrialProd, 1),
      frag_ea_std = as.numeric(scale(frag_ea_c)),
      source = source_label
    )
  
  return(ea_reg_df)
}



run_static_ea_regression <- function(ea_reg_df, source_label, nw_lag = 6) {
  # =========================================================================
  # DESCRIPTION
  # Function to estimate the static euro-area monetary transmission regression
  # with fiscal fragmentation interaction.
  # The coefficient of interest is the interaction between MPS and fiscal
  # fragmentation. Newey-West standard errors are used to account for
  # autocorrelation and heteroskedasticity in monthly time-series residuals.
  # The function also estimates an alternative version using standardized
  # fragmentation, so that the interaction coefficient is interpreted for a
  # one-standard-deviation increase in fragmentation.
  # -------------------------------------------------------------------------
  # INPUTS
  #     ea_reg_df: euro-area regression dataframe.
  #     source_label: label identifying the construction of euro-area variables.
  #     nw_lag: lag length used in the Newey-West variance-covariance matrix.
  #
  # OUTPUT
  #     list containing:
  #       reg: regression using centered fragmentation frag_ea_c
  #       vcov: Newey-West covariance matrix for reg
  #       reg_std: regression using standardized fragmentation frag_ea_std
  #       vcov_std: Newey-West covariance matrix for reg_std
  # -------------------------------------------------------------------------
  
  reg <- lm(
    d_IndustrialProd ~ MPS * frag_ea_c +
      Labor + Financials + Turnover + Prices + Confidence,
    data = ea_reg_df
  )
  
  vcov_nw <- NeweyWest(
    reg,
    lag = nw_lag,
    prewhite = FALSE,
    adjust = TRUE
  )
  
  cat("\n======================================================\n")
  cat(">>> STATIC EURO AREA REGRESSION:", source_label, "\n")
  cat(">>> Newey-West SEs, lag =", nw_lag, "\n")
  cat("======================================================\n")
  print(coeftest(reg, vcov = vcov_nw))
  
  reg_std <- lm(
    d_IndustrialProd ~ MPS * frag_ea_std +
      Labor + Financials + Turnover + Prices + Confidence,
    data = ea_reg_df
  )
  
  vcov_nw_std <- NeweyWest(
    reg_std,
    lag = nw_lag,
    prewhite = FALSE,
    adjust = TRUE
  )
  
  cat("\n======================================================\n")
  cat(">>> STATIC EURO AREA REGRESSION WITH STANDARDIZED FRAGMENTATION:", source_label, "\n")
  cat(">>> Newey-West SEs, lag =", nw_lag, "\n")
  cat("======================================================\n")
  print(coeftest(reg_std, vcov = vcov_nw_std))
  
  return(list(
    reg = reg,
    vcov = vcov_nw,
    reg_std = reg_std,
    vcov_std = vcov_nw_std
  ))
}



estimate_ea_lp <- function(df, y_var, shock_var, state_var, controls,
                           max_h = 24, nw_lag = 6) {
  # =========================================================================
  # DESCRIPTION
  # Function to estimate euro-area local projections with an interaction between
  # monetary policy surprises and fiscal fragmentation.
  # For each horizon h, the dependent variable is defined as:
  #     y_{t+h} - y_{t-1}
  # The coefficient on MPS gives the response at average fragmentation when the
  # state variable is standardized. The coefficient on the interaction gives the
  # change in the response when fragmentation increases by one standard deviation.
  # -------------------------------------------------------------------------
  # INPUTS
  #     df: euro-area regression dataframe.
  #     y_var: name of the response variable.
  #     shock_var: name of the monetary policy surprise variable.
  #     state_var: name of the fragmentation state variable.
  #     controls: vector of control variable names.
  #     max_h: maximum local projection horizon.
  #     nw_lag: lag length used in Newey-West standard errors.
  #
  # OUTPUT
  #     results: list containing one model and Newey-West covariance matrix per
  #              horizon.
  # -------------------------------------------------------------------------
  
  results <- lapply(0:max_h, function(h) {
    
    df_h <- df %>%
      arrange(Date) %>%
      mutate(
        y_lead = dplyr::lead(.data[[y_var]], h),
        y_lag1 = dplyr::lag(.data[[y_var]], 1),
        lhs = y_lead - y_lag1
      )
    
    rhs_terms <- c(
      shock_var,
      paste0(shock_var, ":", state_var),
      controls
    )
    
    fml <- as.formula(
      paste0("lhs ~ ", paste(rhs_terms, collapse = " + "))
    )
    
    mod <- lm(fml, data = df_h)
    
    vc <- NeweyWest(
      mod,
      lag = nw_lag,
      prewhite = FALSE,
      adjust = TRUE
    )
    
    list(
      h = h,
      model = mod,
      vcov = vc
    )
  })
  
  return(results)
}



extract_lp_irf_terms <- function(results, shock_var, state_var) {
  # =========================================================================
  # DESCRIPTION
  # Function to extract the local projection coefficients needed to construct
  # conditional impulse response functions.
  # For each horizon, the function extracts:
  #   - beta_h: coefficient on the monetary policy shock
  #   - gamma_h: coefficient on the shock x fragmentation interaction
  #   - variance of beta_h
  #   - variance of gamma_h
  #   - covariance between beta_h and gamma_h
  # These quantities are then used to compute conditional IRFs and confidence
  # intervals with the delta method.
  # -------------------------------------------------------------------------
  # INPUTS
  #     results: list returned by estimate_ea_lp().
  #     shock_var: name of the monetary policy surprise variable.
  #     state_var: name of the fragmentation state variable.
  #
  # OUTPUT
  #     dataframe with h, beta, gamma, var_beta, var_gamma and cov_bg.
  # -------------------------------------------------------------------------
  
  interaction_name_1 <- paste0(shock_var, ":", state_var)
  interaction_name_2 <- paste0(state_var, ":", shock_var)
  
  out <- lapply(results, function(res) {
    
    mod <- res$model
    vc  <- res$vcov
    h   <- res$h
    
    cn <- names(coef(mod))
    
    int_name <- if (interaction_name_1 %in% cn) {
      interaction_name_1
    } else if (interaction_name_2 %in% cn) {
      interaction_name_2
    } else {
      NA_character_
    }
    
    beta <- if (shock_var %in% cn) coef(mod)[shock_var] else NA_real_
    gamma <- if (!is.na(int_name)) coef(mod)[int_name] else NA_real_
    
    var_beta <- if (shock_var %in% rownames(vc)) {
      vc[shock_var, shock_var]
    } else {
      NA_real_
    }
    
    var_gamma <- if (!is.na(int_name) && int_name %in% rownames(vc)) {
      vc[int_name, int_name]
    } else {
      NA_real_
    }
    
    cov_bg <- if (!is.na(int_name) && shock_var %in% rownames(vc) && int_name %in% colnames(vc)) {
      vc[shock_var, int_name]
    } else {
      NA_real_
    }
    
    data.frame(
      h = h,
      beta = beta,
      gamma = gamma,
      var_beta = var_beta,
      var_gamma = var_gamma,
      cov_bg = cov_bg
    )
  })
  
  bind_rows(out)
}



build_irf_state <- function(lp_terms, state_value, state_label, source_label) {
  # =========================================================================
  # DESCRIPTION
  # Function to build a conditional IRF for a given value of the fragmentation
  # state variable.
  # With standardized fragmentation, the state values are:
  #   - -1 for low fragmentation
  #   -  0 for average fragmentation
  #   - +1 for high fragmentation
  # The IRF is computed as:
  #     IRF_h(state) = beta_h + gamma_h x state
  # -------------------------------------------------------------------------
  # INPUTS
  #     lp_terms: dataframe returned by extract_lp_irf_terms().
  #     state_value: numeric value of the state variable.
  #     state_label: label used in plots.
  #     source_label: label identifying the construction of euro-area variables.
  #
  # OUTPUT
  #     dataframe containing h, IRF, standard error and confidence bands.
  # -------------------------------------------------------------------------
  
  lp_terms %>%
    mutate(
      irf = beta + gamma * state_value,
      se_irf = sqrt(
        var_beta +
          (state_value^2) * var_gamma +
          2 * state_value * cov_bg
      ),
      ci_low = irf - 1.96 * se_irf,
      ci_high = irf + 1.96 * se_irf,
      ci_low_68 = irf - 1.0 * se_irf,
      ci_high_68 = irf + 1.0 * se_irf,
      state = state_label,
      source = source_label
    ) %>%
    select(source, h, state, irf, se_irf, ci_low, ci_high, ci_low_68, ci_high_68)
}



run_ea_lps <- function(ea_reg_df, source_label, controls, max_h = 24, nw_lag = 6) {
  # =========================================================================
  # DESCRIPTION
  # Function to estimate local projections and build all dynamic objects used in
  # the monetary policy fragmentation analysis.
  # The function estimates LPs with standardized fragmentation, so that +1 = +1 sd, 
  # extracts beta_h and gamma_h, builds conditional IRFs for low, average and high
  # fragmentation, constructs the differential IRF high minus low, and stores the
  # interaction coefficient path gamma_h.
  # -------------------------------------------------------------------------
  # INPUTS
  #     ea_reg_df: euro-area regression dataframe.
  #     source_label: label identifying the construction of euro-area variables.
  #     controls: vector of control variable names.
  #     max_h: maximum local projection horizon.
  #     nw_lag: lag length used in Newey-West standard errors.
  #
  # OUTPUT
  #     list containing:
  #       lp_results: raw LP models and covariance matrices
  #       lp_terms: extracted beta_h and gamma_h terms
  #       irf_all: conditional IRFs for low, average and high fragmentation
  #       irf_diff: high-minus-low differential IRF
  #       gamma_df: interaction coefficient path
  # -------------------------------------------------------------------------
  
  ea_lp_df <- ea_reg_df %>%
    arrange(Date) %>%
    mutate(
      frag_ea_std = as.numeric(scale(frag_ea_c))
    )
  
  lp_results <- estimate_ea_lp(
    df        = ea_lp_df,
    y_var     = "IndustrialProd",
    shock_var = "MPS",
    state_var = "frag_ea_std",
    controls  = controls,
    max_h     = max_h,
    nw_lag    = nw_lag
  )
  
  lp_terms <- extract_lp_irf_terms(
    results   = lp_results,
    shock_var = "MPS",
    state_var = "frag_ea_std"
  )
  
  irf_low <- build_irf_state(
    lp_terms,
    state_value = -1,
    state_label = "Low fragmentation (-1 sd)",
    source_label = source_label
  )
  
  irf_mean <- build_irf_state(
    lp_terms,
    state_value = 0,
    state_label = "Average fragmentation",
    source_label = source_label
  )
  
  irf_high <- build_irf_state(
    lp_terms,
    state_value = 1,
    state_label = "High fragmentation (+1 sd)",
    source_label = source_label
  )
  
  irf_all <- bind_rows(irf_low, irf_mean, irf_high)
  
  gamma_df <- lp_terms %>%
    mutate(
      estimate = gamma,
      se = sqrt(var_gamma),
      ci_low = estimate - 1.96 * se,
      ci_high = estimate + 1.96 * se,
      ci_low_68 = estimate - 1.0 * se,
      ci_high_68 = estimate + 1.0 * se,
      source = source_label
    ) %>%
    select(source, h, estimate, se, ci_low, ci_high, ci_low_68, ci_high_68)
  
  return(list(
    lp_results = lp_results,
    lp_terms = lp_terms,
    irf_all = irf_all,
    gamma_df = gamma_df
  ))
}



build_static_marginal_effect <- function(reg_object, vcov_object, ea_reg_df, source_label) {
  # =========================================================================
  # DESCRIPTION
  # Function to compute the marginal effect of monetary policy surprises over
  # the observed range of euro-area fiscal fragmentation.
  # The marginal effect is:
  #     d Delta Y_t / d MPS_t = beta_MPS + gamma x frag_ea_c
  # Standard errors are computed by the delta method using the supplied
  # variance-covariance matrix.
  # -------------------------------------------------------------------------
  # INPUTS
  #     reg_object: static regression object.
  #     vcov_object: variance-covariance matrix, usually Newey-West.
  #     ea_reg_df: euro-area regression dataframe.
  #     source_label: label identifying the construction of euro-area variables.
  #
  # OUTPUT
  #     dataframe containing the marginal effect and confidence bands over the
  #     fragmentation grid.
  # -------------------------------------------------------------------------
  
  b <- coef(reg_object)
  V <- vcov_object
  
  beta_mps  <- b["MPS"]
  gamma_int <- b["MPS:frag_ea_c"]
  
  frag_grid <- seq(
    min(ea_reg_df$frag_ea_c, na.rm = TRUE),
    max(ea_reg_df$frag_ea_c, na.rm = TRUE),
    length.out = 100
  )
  
  marginal_effect <- beta_mps + gamma_int * frag_grid
  
  se_marginal <- sqrt(
    V["MPS", "MPS"] +
      frag_grid^2 * V["MPS:frag_ea_c", "MPS:frag_ea_c"] +
      2 * frag_grid * V["MPS", "MPS:frag_ea_c"]
  )
  
  data.frame(
    source = source_label,
    fragmentation_index = frag_grid + 100,
    frag_ea_c = frag_grid,
    marginal_effect = marginal_effect,
    ci_low = marginal_effect - 1.96 * se_marginal,
    ci_high = marginal_effect + 1.96 * se_marginal,
    ci_low_68 = marginal_effect - 1.0 * se_marginal,
    ci_high_68 = marginal_effect + 1.0 * se_marginal
  )
}



build_coef_comparison <- function(static_weighted, static_direct) {
  # =========================================================================
  # DESCRIPTION
  # Function to build a comparison table of static regression coefficients for
  # the two euro-area specifications:
  #   1. GDP-weighted country aggregates
  #   2. Direct euro-area EA-MD factors
  # The table uses Newey-West standard errors stored in the regression objects.
  # -------------------------------------------------------------------------
  # INPUTS
  #     static_weighted: output from run_static_ea_regression() for the
  #                      GDP-weighted country aggregate specification.
  #     static_direct: output from run_static_ea_regression() for the direct
  #                    euro-area EA-MD specification.
  #
  # OUTPUT
  #     dataframe containing estimates, Newey-West standard errors, t-statistics
  #     and p-values.
  # -------------------------------------------------------------------------
  
  coef_comparison <- bind_rows(
    data.frame(
      specification = "GDP-weighted country aggregates",
      coefficient = names(coef(static_weighted$reg)),
      estimate = coef(static_weighted$reg),
      se_nw = sqrt(diag(static_weighted$vcov))
    ),
    data.frame(
      specification = "Direct euro-area EA-MD factors",
      coefficient = names(coef(static_direct$reg)),
      estimate = coef(static_direct$reg),
      se_nw = sqrt(diag(static_direct$vcov))
    )
  ) %>%
    mutate(
      t_stat = estimate / se_nw,
      p_value = 2 * pt(abs(t_stat), df = Inf, lower.tail = FALSE)
    )
  
  return(coef_comparison)
}



diagnose_autocorr_extensive <- function(reg_object, source_label, max_lag = 30) {
  # =========================================================================
  # DESCRIPTION
  # Function to perform an extensive residual autocorrelation diagnostic.
  # It computes Ljung-Box test statistics and p-values for a sequence of lags
  # from 1 to max_lag to identify the exact persistence of autocorrelation.
  # This helps justify the choice of bandwidth for Newey-West estimators.
  # -------------------------------------------------------------------------
  # INPUTS
  #      reg_object: lm regression object, static or LP horizon.
  #      source_label: character. Label of the specification for output.
  #      max_lag: integer. Maximum lag for the ACF and Ljung-Box scan.
  #
  # OUTPUT
  #      lb_results: dataframe containing p-values for each lag.
  # -------------------------------------------------------------------------
  
  res <- residuals(reg_object)
  res <- res[is.finite(res)]
  
  cat("\n======================================================\n")
  cat(">>> RESIDUAL AUTOCORRELATION SCAN (1-", max_lag, "): ", source_label, "\n", sep = "")
  cat("======================================================\n")
  
  # 1. Compute Ljung-Box p-values for every lag from 1 to max_lag
  lb_results <- data.frame(lag = 1:max_lag) %>%
    rowwise() %>%
    mutate(
      statistic = Box.test(res, lag = lag, type = "Ljung-Box")$statistic,
      p_value = Box.test(res, lag = lag, type = "Ljung-Box")$p.value
    ) %>%
    ungroup()
  
  # 2. Identify the last lag where autocorrelation remains significant
  significant_lags <- lb_results %>% filter(p_value < 0.05)
  
  if (nrow(significant_lags) > 0) {
    max_sig_lag <- max(significant_lags$lag)
    cat("(!) Significant autocorrelation detected up to lag:", max_sig_lag, "\n")
  } else {
    cat("OK: No significant autocorrelation detected at the 5% level.\n")
  }
  
  print(as.data.frame(lb_results))
  
  return(lb_results)
}