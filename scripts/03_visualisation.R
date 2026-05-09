# ==============================================================================
# SCRIPT 03: VISUALIZATION & REPORTING
# ==============================================================================
# This script loads the nowcast results (.rds) and generates 
# comparative plots and standardized grids for the final report.

# ---- 0. Environment Setup ----
if (!exists("pseudo_real_time")) source("_setup.R")

# Load the master data bundle (to get realized/actual series for comparison)
data_master <- readRDS(here("data/processed/data_full_treatments.rds"))

# ---- 1. Main Visualization Loop ----
cat("\n======================================================\n")
cat(">>> STARTING VISUALIZATION PIPELINE\n")
cat("======================================================\n")

for (block in list_blocks) {
  
  # Initialize a list to store plots for the current block (to create a grid later)
  plot_list <- list()
  
  cat(sprintf("\n--- Generating plots for: %s ---\n", block))
  
  for (country in list_countries) {
    
    # Step A: Load Nowcast Results 
    list_models <- c("factor_midas", "sg_lasso", "ar1", "rw")
    nowcast_res_list <- list()
    for (model_name in list_models) {
      file_name_rds <- paste0("nowcast_", model_name, "_", tolower(country), "_", tolower(block), ".rds")
      path_rds <- here("data/processed", file_name_rds)
      if (file.exists(path_rds)) {
        nowcast_res_list[[model_name]] <- readRDS(path_rds)
      }
    }
    
    if (length(nowcast_res_list) == 0) {
      warning(sprintf("!!! [SKIP] No results found for %s - %s. Run script 02 first.", country, block))
      next
    }
    
    nowcast_res <- do.call(rbind, nowcast_res_list)
    
    # Step B: Comparison Data (Realized vs Nowcast) 
    # We need the ragged_df to show the "Actual" line in the plot
    df_publi_delay <- read_excel(here("data/publication_delay.xlsx"), sheet = country)
    colnames(df_publi_delay) <- trimws(colnames(df_publi_delay))
    df_statio <- data_master$statio[[country]][[block]][-(1:3), ]
    if (block == "Expenditures" && "Sovereign credit rating revisions" %in% colnames(df_statio)) {
      df_statio[["Sovereign credit rating revisions"]] <- NULL
    }
    ragged_df <- ragged_edge_dataset(df_statio, df_publi_delay)
    
    # Step C: Individual Plot
    p <- plot_nowcast_vs_realized(
      nowcast_df   = nowcast_res,
      ragged_df    = ragged_df,
      country_name = country
    )
    
    # Step D: Customize for Grid Layout 
    p <- p + theme(
      plot.title    = element_text(size = 12, face = "bold"),
      plot.subtitle = element_blank(),
      axis.title.x  = element_blank(),
      axis.title.y  = element_blank()
    )
    
    plot_list[[country]] <- p
  }
  
  # Step E: 2x2 Grid 
  if (length(plot_list) > 0) {
    
    # Patchwork magic: combine all plots in the list
    combined_plot <- wrap_plots(plot_list, ncol = 2) + 
      plot_annotation(
        title = paste("Nowcast vs Realized Series -", block),
        subtitle = "Standardized values. Realized series is repeated over the corresponding quarter.",
        theme = theme(
          plot.title    = element_text(size = 16, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey30")
        )
      ) +
      plot_layout(guides = "collect") & theme(legend.position = "bottom")
    
    # Save the combined grid as PDF
    output_name <- paste0("combined_nowcast_", tolower(block), ".pdf")
    ggsave(
      filename = here("outputs/figures/nowcast", output_name),
      plot     = combined_plot,
      width    = 15, 
      height   = 10,
      dpi      = 300
    )
    
    cat(sprintf(">>> [OK] Grid saved: %s\n", output_name))
  }
}

cat("\n======================================================\n")
cat("VISUALIZATION COMPLETED\n")
cat("See 'graphs/nowcast/' folder.\n")
cat("======================================================\n")