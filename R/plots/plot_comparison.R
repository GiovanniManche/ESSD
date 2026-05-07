plot_comparison_pdf <- function(countries_raw, countries_statio, series_name, type = "Revenues") {
  
  pdf_name <- here("graphs", paste0("Comparison_", series_name, ".pdf"))
  dir.create(here("graphs"), showWarnings = FALSE)
  pdf(file = pdf_name, width = 14, height = 10)
  
  # 4 countries x 2 (raw + statio) = 8 plots
  par(mfrow = c(4, 2), mar = c(3, 3, 3, 1), oma = c(0, 0, 3, 0))
  
  for (country_name in names(countries_raw)) {
    df_raw  <- countries_raw[[country_name]][[type]]
    df_stat <- countries_statio[[country_name]][[type]]
    
    dates  <- df_raw$Dates
    x_raw  <- df_raw[[series_name]]
    x_stat <- df_stat[[series_name]]
    
    idx_raw  <- !is.na(x_raw)
    idx_stat <- !is.na(x_stat)
    
    # --- Raw ---
    if (sum(idx_raw) == 0) {
      plot.new(); title(main = paste(country_name, "-", series_name, "- Raw"))
      text(0.5, 0.5, "100% NA", col = "red", cex = 1.5)
    } else {
      plot(dates[idx_raw], x_raw[idx_raw], type = "l", col = "steelblue",
           main = paste(country_name, "- Raw"), xlab = "", ylab = "")
    }
    
    # --- Stationary ---
    if (sum(idx_stat) == 0) {
      plot.new(); title(main = paste(country_name, "-", series_name, "- Stationary"))
      text(0.5, 0.5, "100% NA", col = "red", cex = 1.5)
    } else {
      plot(dates[idx_stat], x_stat[idx_stat], type = "l", col = "darkorange",
           main = paste(country_name, "- Stationary"), xlab = "", ylab = "")
    }
  }
  
  mtext(paste(series_name), outer = TRUE, cex = 1.2, line = 1)
  dev.off()
  cat("Saved:", pdf_name, "\n")
}