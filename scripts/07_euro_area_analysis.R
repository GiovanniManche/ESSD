# ==============================================================================
# SCRIPT 07: EURO AREA REGRESSIONS AND LOCAL PROJECTIONS
# ==============================================================================

# ---- 0. Common inputs ----

static_lag_weighted <- 24
static_lag_direct   <- 14
lp_lag              <- 12
max_h               <- 24

mps_data <- read_excel(
  here("data/EA-EMDP.xlsx"),
  sheet = "emdp"
)

df_mps <- mps_builder(mps_data)

ea_frag <- euro_index %>%
  filter(model == "sg_lasso", block == "Total") %>%
  transmute(
    year_month = as.character(year_month),
    fragmentation_index = fragmentation_index,
    frag_ea_c = fragmentation_index - 100
  )

controls <- c(
  "Labor",
  "Financials",
  "Turnover",
  "Prices",
  "Confidence"
)

code_to_country <- c(
  DE = "germany",
  ES = "spain",
  FR = "france",
  IT = "italy",
  EA = "ea"
)

files <- list.files(
  here("data/EA_MD_DB"),
  pattern = "\\.xlsx$",
  full.names = TRUE
)

for(f in files){
  base_name <- tools::file_path_sans_ext(basename(f))
  code <- substr(base_name, 1, 2)
  
  if(code %in% names(code_to_country)){
    country_name <- code_to_country[[code]]
    obj_name <- paste0("EA_MD_", country_name)
    
    sheets_of_country <- list(
      data = read_excel(f, sheet = "data"),
      info = read_excel(f, sheet = "info")
    )
    
    assign(obj_name, sheets_of_country, envir = .GlobalEnv)
  }
}

countries_ea_md <- list(
  Germany = EA_MD_germany,
  Italy   = EA_MD_italy,
  France  = EA_MD_france,
  Spain   = EA_MD_spain
)

list_factors <- factors_list_ea_md(
  global_list = countries_ea_md,
  freq = "M"
)


# ---- 1. Specify the two specifications ----

# Baseline: GDP-weighted average of country-level EA-MD factors.
# Robustness: direct euro area EA-MD factors.

ea_reg_df_weighted <- build_ea_regression_data(
  source_type = "weighted_4countries",
  list_factors = list_factors,
  countries_ea_md = countries_ea_md,
  gdp_long = gdp_long,
  EA_MD_ea = EA_MD_ea,
  df_mps = df_mps,
  ea_frag = ea_frag
)

ea_reg_df_direct <- build_ea_regression_data(
  source_type = "direct_ea",
  list_factors = list_factors,
  countries_ea_md = countries_ea_md,
  gdp_long = gdp_long,
  EA_MD_ea = EA_MD_ea,
  df_mps = df_mps,
  ea_frag = ea_frag
)


# ---- 1b. Dataset checks ----

cat("\n======================================================\n")
cat(">>> CHECK DATASET: GDP-WEIGHTED COUNTRY AGGREGATES\n")
cat("======================================================\n")
print(summary(ea_reg_df_weighted$fragmentation_index))
print(summary(ea_reg_df_weighted$MPS))
print(summary(ea_reg_df_weighted$d_IndustrialProd))
print(
  ea_reg_df_weighted %>%
    summarize(
      n_obs = sum(!is.na(d_IndustrialProd) & !is.na(MPS) & !is.na(frag_ea_c)),
      cor_mps_frag = cor(MPS, frag_ea_c, use = "complete.obs")
    )
)

cat("\n======================================================\n")
cat(">>> CHECK DATASET: DIRECT EURO AREA EA-MD FACTORS\n")
cat("======================================================\n")
print(summary(ea_reg_df_direct$fragmentation_index))
print(summary(ea_reg_df_direct$MPS))
print(summary(ea_reg_df_direct$d_IndustrialProd))
print(
  ea_reg_df_direct %>%
    summarize(
      n_obs = sum(!is.na(d_IndustrialProd) & !is.na(MPS) & !is.na(frag_ea_c)),
      cor_mps_frag = cor(MPS, frag_ea_c, use = "complete.obs")
    )
)


# ---- 2. Static linear regressions ----
# ΔF_prod = a + b*MPS + c*Frag + d*MPS*Frag + gamma*controls + e_t

static_weighted <- run_static_ea_regression(
  ea_reg_df = ea_reg_df_weighted,
  source_label = "GDP-weighted country aggregates",
  nw_lag = static_lag_weighted
)

static_direct <- run_static_ea_regression(
  ea_reg_df = ea_reg_df_direct,
  source_label = "Direct euro area EA-MD factors",
  nw_lag = static_lag_direct
)


# ---- 2b. Residual autocorrelation diagnostics ----

lb_weighted <- diagnose_autocorr_extensive(
  reg_object = static_weighted$reg,
  source_label = "GDP-weighted country aggregates",
  max_lag = 30
)

lb_direct <- diagnose_autocorr_extensive(
  reg_object = static_direct$reg,
  source_label = "Direct euro area EA-MD factors",
  max_lag = 30
)


# ---- 3. Local projections ----

lp_weighted <- run_ea_lps(
  ea_reg_df = ea_reg_df_weighted,
  source_label = "GDP-weighted country aggregates",
  controls = controls,
  max_h = max_h,
  nw_lag = lp_lag
)

lp_direct <- run_ea_lps(
  ea_reg_df = ea_reg_df_direct,
  source_label = "Direct euro area EA-MD factors",
  controls = controls,
  max_h = max_h,
  nw_lag = lp_lag
)


# ---- 4. Combine outputs ----

irf_all_both <- bind_rows(
  lp_weighted$irf_all,
  lp_direct$irf_all
)

gamma_both <- bind_rows(
  lp_weighted$gamma_df,
  lp_direct$gamma_df
)

marginal_weighted <- build_static_marginal_effect(
  reg_object = static_weighted$reg,
  vcov_object = static_weighted$vcov,
  ea_reg_df = ea_reg_df_weighted,
  source_label = "GDP-weighted country aggregates"
)

marginal_direct <- build_static_marginal_effect(
  reg_object = static_direct$reg,
  vcov_object = static_direct$vcov,
  ea_reg_df = ea_reg_df_direct,
  source_label = "Direct euro area EA-MD factors"
)

marginal_both <- bind_rows(
  marginal_weighted,
  marginal_direct
)


# ---- 5. Plots ----

fig_dir <- here("outputs", "figures", "monetary_policy")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)


# ---- 5.1 MPS series ----

p_mps <- ggplot(ea_reg_df_weighted, aes(x = Date, y = MPS)) +
  geom_col(na.rm = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  theme_minimal() +
  labs(
    title = "ECB monetary policy surprises",
    subtitle = "Target + Path surprise measure",
    x = "Date",
    y = "MPS"
  )

ggsave(
  filename = file.path(fig_dir, "mps_series_euro_area.pdf"),
  plot = p_mps,
  width = 9,
  height = 5,
  dpi = 300
)


# ---- 5.2 Combined marginal effects ----

p_marginal_both <- ggplot(marginal_both, aes(x = fragmentation_index, y = marginal_effect)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high, fill = source), alpha = 0.15, color = NA) +
  geom_ribbon(aes(ymin = ci_low_68, ymax = ci_high_68, fill = source), alpha = 0.2, color = NA) +
  geom_line(aes(color = source), linewidth = 0.9) +
  facet_wrap(~ source, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Marginal effect of MPS by euro area fiscal fragmentation",
    subtitle = paste0(
      "Static regressions, Newey-West bands | weighted lag = ",
      static_lag_weighted,
      ", direct EA lag = ",
      static_lag_direct
    ),
    x = "Euro area fiscal fragmentation index",
    y = "Marginal effect of MPS on ΔF_production",
    color = "Specification",
    fill = "Specification"
  )

ggsave(
  filename = file.path(fig_dir, "marginal_effect_both_specs.pdf"),
  plot = p_marginal_both,
  width = 12,
  height = 5.5,
  dpi = 300
)


# ---- 5.3 Combined gamma_h ----

p_gamma_both <- ggplot(gamma_both, aes(x = h, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red3") +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high, fill = source), alpha = 0.15, color = NA) +
  geom_line(aes(color = source), linewidth = 0.9) +
  geom_point(aes(color = source), size = 1.4) +
  facet_wrap(~ source, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Euro area LPs: interaction between MPS and fiscal fragmentation",
    subtitle = paste0("Gamma_h from local projections, Newey-West bands, lag = ", lp_lag),
    x = "Horizon",
    y = expression(gamma[h]),
    color = "Specification",
    fill = "Specification"
  )

ggsave(
  filename = file.path(fig_dir, "gamma_lp_both_specs.pdf"),
  plot = p_gamma_both,
  width = 12,
  height = 5.5,
  dpi = 300
)


# ---- 5.4 Combined conditional IRFs ----

p_irf_both <- ggplot(irf_all_both, aes(x = h, y = irf, color = state, fill = state)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  # 95% bands
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.1, color = NA) +
  # 68%
  geom_ribbon(aes(ymin = ci_low_68, ymax = ci_high_68), alpha = 0.25, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.3) +
  facet_wrap(~ source, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Euro area IRFs to a monetary policy surprise",
    subtitle = paste0("Local projections with MPS × fragmentation, Newey-West bands, lag = ", lp_lag),
    x = "Horizon",
    y = "Response of F_production",
    color = "State",
    fill = "State"
  )

ggsave(
  filename = file.path(fig_dir, "irf_conditional_both_specs.pdf"),
  plot = p_irf_both,
  width = 12,
  height = 6,
  dpi = 300
)


# ---- 5.5 GDP-weighted standalone graphs ----

marginal_weighted_only <- marginal_both %>%
  filter(source == "GDP-weighted country aggregates")

gamma_weighted_only <- gamma_both %>%
  filter(source == "GDP-weighted country aggregates")

irf_weighted_only <- irf_all_both %>%
  filter(source == "GDP-weighted country aggregates")

p_marginal_weighted <- ggplot(marginal_weighted_only, aes(x = fragmentation_index, y = marginal_effect)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.18) +
  geom_line(linewidth = 0.9) +
  theme_minimal() +
  labs(
    title = "Marginal effect of MPS by euro area fiscal fragmentation",
    subtitle = paste0("GDP-weighted country aggregates, Newey-West bands, lag = ", static_lag_weighted),
    x = "Euro area fiscal fragmentation index",
    y = "Marginal effect of MPS on ΔF_production"
  )

ggsave(
  filename = file.path(fig_dir, "marginal_effect_gdp_weighted.pdf"),
  plot = p_marginal_weighted,
  width = 9,
  height = 5.5,
  dpi = 300
)

p_gamma_weighted <- ggplot(gamma_weighted_only, aes(x = h, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.18) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.4) +
  theme_minimal() +
  labs(
    title = "Euro area LPs: interaction between MPS and fiscal fragmentation",
    subtitle = paste0("GDP-weighted country aggregates, Newey-West bands, lag = ", lp_lag),
    x = "Horizon",
    y = expression(gamma[h])
  )

ggsave(
  filename = file.path(fig_dir, "gamma_lp_gdp_weighted.pdf"),
  plot = p_gamma_weighted,
  width = 9,
  height = 5.5,
  dpi = 300
)

p_irf_weighted <- ggplot(irf_weighted_only, aes(x = h, y = irf, color = state, fill = state)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.3) +
  theme_minimal() +
  labs(
    title = "Euro area IRFs to a monetary policy surprise",
    subtitle = paste0("GDP-weighted country aggregates, Newey-West bands, lag = ", lp_lag),
    x = "Horizon",
    y = "Response of F_production",
    color = "State",
    fill = "State"
  )

ggsave(
  filename = file.path(fig_dir, "irf_conditional_gdp_weighted.pdf"),
  plot = p_irf_weighted,
  width = 9,
  height = 6,
  dpi = 300
)

# ---- 6. Save estimation results ----


table_dir <- here("outputs", "tables", "monetary_policy")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)


# ---- 6.1 Helper: tidy coeftest output ----

tidy_coeftest <- function(reg_object, vcov_object, specification, nw_lag) {
  
  ct <- lmtest::coeftest(reg_object, vcov = vcov_object)
  
  out <- data.frame(
    coefficient   = rownames(ct),
    estimate      = ct[, 1],
    std_error     = ct[, 2],
    t_stat        = ct[, 3],
    p_value       = ct[, 4],
    specification = specification,
    nw_lag        = nw_lag,
    row.names     = NULL
  )
  
  out <- out %>%
    select(specification, nw_lag, coefficient, estimate, std_error, t_stat, p_value)
  
  return(out)
}


# ---- 6.2 Static regression results ----

static_results_weighted <- tidy_coeftest(
  reg_object = static_weighted$reg,
  vcov_object = static_weighted$vcov,
  specification = "GDP-weighted country aggregates",
  nw_lag = static_lag_weighted
)

static_results_direct <- tidy_coeftest(
  reg_object = static_direct$reg,
  vcov_object = static_direct$vcov,
  specification = "Direct euro area EA-MD factors",
  nw_lag = static_lag_direct
)

static_results <- bind_rows(
  static_results_weighted,
  static_results_direct
)

write.csv(
  static_results,
  file.path(table_dir, "static_regression_results.csv"),
  row.names = FALSE
)


# ---- 6.3 Static regression results with standardized fragmentation ----

static_results_std_weighted <- tidy_coeftest(
  reg_object = static_weighted$reg_std,
  vcov_object = static_weighted$vcov_std,
  specification = "GDP-weighted country aggregates",
  nw_lag = static_lag_weighted
)

static_results_std_direct <- tidy_coeftest(
  reg_object = static_direct$reg_std,
  vcov_object = static_direct$vcov_std,
  specification = "Direct euro area EA-MD factors",
  nw_lag = static_lag_direct
)

static_results_std <- bind_rows(
  static_results_std_weighted,
  static_results_std_direct
)

write.csv(
  static_results_std,
  file.path(table_dir, "static_regression_results_standardized_frag.csv"),
  row.names = FALSE
)


# ---- 6.4 Local projection interaction coefficients gamma_h ----

write.csv(
  gamma_both,
  file.path(table_dir, "lp_gamma_interaction_coefficients.csv"),
  row.names = FALSE
)


# ---- 6.5 Conditional IRFs ----

write.csv(
  irf_all_both,
  file.path(table_dir, "lp_conditional_irfs.csv"),
  row.names = FALSE
)



# ---- 6.7 Static marginal effects ----

write.csv(
  marginal_both,
  file.path(table_dir, "static_marginal_effects_by_fragmentation.csv"),
  row.names = FALSE
)


# ---- 6.8 Residual autocorrelation diagnostics ----

lb_weighted <- lb_weighted %>%
  mutate(specification = "GDP-weighted country aggregates") %>%
  select(specification, everything())

lb_direct <- lb_direct %>%
  mutate(specification = "Direct euro area EA-MD factors") %>%
  select(specification, everything())

lb_results <- bind_rows(
  lb_weighted,
  lb_direct
)

write.csv(
  lb_results,
  file.path(table_dir, "residual_autocorrelation_ljung_box.csv"),
  row.names = FALSE
)


# Save everything into one Excel workbook 
coef_comparison <- build_coef_comparison(static_weighted, static_direct)
if (requireNamespace("writexl", quietly = TRUE)) {
  
  writexl::write_xlsx(
    list(
      static_results = static_results,
      static_results_std = static_results_std,
      coefficient_comparison = coef_comparison,
      lp_gamma = gamma_both,
      conditional_irfs = irf_all_both,
      marginal_effects = marginal_both,
      ljung_box = lb_results
    ),
    path = file.path(table_dir, "monetary_policy_results.xlsx")
  )
  
  cat("\n>>> Excel workbook saved in:", file.path(table_dir, "monetary_policy_results.xlsx"), "\n")
  
} else {
  
  cat("\n>>> Package 'writexl' is not installed. CSV files were saved only.\n")
}


cat("\n======================================================\n")
cat(">>> Tables saved in:", table_dir, "\n")
cat("======================================================\n")