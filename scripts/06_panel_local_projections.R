# ==============================================================================
# SCRIPT 06: PANEL LOCAL PROJECTIONS
# ==============================================================================
# This script builds EA-MD macro-financial factors, adds monetary policy
# surprises and country-level state variables, constructs a country-level panel,
# and estimates panel local projections.
# ==============================================================================


# ---- 1. Import EA-MD data ----

code_to_country <- c(
  DE = "germany",
  ES = "spain",
  FR = "france",
  IT = "italy"
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


# ---- 2. Build country list ----

countries_ea_md <- list(
  Germany = EA_MD_germany,
  Italy   = EA_MD_italy,
  France  = EA_MD_france,
  Spain   = EA_MD_spain
)


# ---- 3. Build EA-MD factors ----

list_factors <- factors_list_ea_md(
  global_list = countries_ea_md,
  freq = "M"
)


# ---- 4. Import monetary policy surprises ----

mps_data <- read_excel(
  here("data/EA-EMDP.xlsx"),
  sheet = "emdp"
)

df_mps <- mps_builder(mps_data)


# ---- 5. Prepare country-level state variable ----

df_frag <- country_monthly_index_extended %>%
  filter(model == "sg_lasso", block == "Total") %>%
  transmute(
    Country = stringr::str_to_title(country),
    year_month = as.character(year_month),
    frag = climate_index_country
  )


# ---- 6. Add monetary policy surprises to country factors ----

for(i in seq_along(list_factors)){
  df_factor <- list_factors[[i]]
  
  df_agg <- time_series_aggregator(
    df_factor,
    df_mps,
    freq = "M"
  )
  
  list_factors[[i]] <- df_agg
}


# ---- 7. Build panel dataframe ----

panel_df <- build_panel(
  list_df = list_factors,
  list_country_name = names(countries_ea_md),
  start_date = list_factors[[1]][["Date"]][50],
  end_date   = list_factors[[1]][["Date"]][300]
)


# ---- 8. Merge state variable into panel ----

panel_df <- panel_df %>%
  mutate(year_month = format(as.Date(Date), "%Y-%m")) %>%
  left_join(df_frag, by = c("Country", "year_month"))


# ---- 9. Check final panel ----

panel_check <- panel_df %>%
  count(Country, is_missing_frag = is.na(frag))

print(panel_check)

summary(panel_df$frag)


# ---- 10. Estimate panel local projections ----

max_h <- 36

controls <- c(
  "Labor",
  "Financials",
  "Turnover",
  "Prices",
  "Confidence"
)

lp_results <- estimate_panel_lp(
  panel_df = panel_df,
  y_var = "IndustrialProd",
  shock_var = "MPS",
  interaction_var = "frag",
  controls = controls,
  max_h = max_h
)


# ---- 11. Extract interaction coefficients ----

gamma <- extract_lp_coef(
  results = lp_results,
  shock_var = "MPS",
  interaction_var = "frag"
)


# ---- 12. Plot local projection coefficients ----
x11();
plot(
  0:max_h,
  gamma,
  type = "b",
  xlab = "Horizon",
  ylab = "Gamma_h",
  main = "Interaction between monetary policy and country-level fragmentation"
)

# ---- 13. Check: correlation between MPS and country-level indices
cor_pooled <- cor(
  panel_df$MPS,
  panel_df$frag,
  use = "complete.obs"
)
cat("Pooled correlation MPS ~ frag :", round(cor_pooled, 4), "\n")


reg_check <- lm(MPS ~ frag, data = panel_df)
summary(reg_check)

panel_df %>%
  group_by(Country) %>%
  summarise(
    cor_mps_frag = cor(MPS, frag, use = "complete.obs"),
    n            = sum(!is.na(MPS) & !is.na(frag))
  ) %>%
  print()