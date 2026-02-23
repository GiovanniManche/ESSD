plot_series_pdf <- function(df_raw, df_stat, country_name, type = "Revenues") {
  # Function to plot the raw series and their stationarized version
  # and save them in a PDF file
  
  
  dates        <- df_raw$Dates
  is_num_col <- sapply(df_raw, is.numeric)
  numeric_cols <- names(df_raw)[is_num_col]
  series_names <- setdiff(numeric_cols, "Dates")
  
  pdf_name <- file.path("graphs", paste0("Statio_", country_name, "_", type, ".pdf"))
  pdf(file = pdf_name, width = 12, height = 8)
  
  # 3 variables (6 plots per page
  par(mfrow = c(3, 2), mar = c(3, 3, 3, 1), oma = c(0, 0, 3, 0))
  
  for (s in series_names) {
    x_raw  <- df_raw[[s]]
    x_stat <- df_stat[[s]]
    
    # Filter out NA 
    idx_raw  <- !is.na(x_raw)
    idx_stat <- !is.na(x_stat)
    
    # Raw series 
    if (sum(idx_raw) == 0) {
      plot.new()
      title(main = paste(s, "- Raw"))
      text(0.5, 0.5, "100% NA", col = "red", cex = 1.5)
    } else {
      plot(dates[idx_raw], x_raw[idx_raw], type = "l",
           main = paste(s, "- Raw"), xlab = "", ylab = "", col = "steelblue")
    }
    
    # Stationarized series
    if (sum(idx_stat) == 0) {
      plot.new()
      title(main = paste(s, "- Stationary"))
      text(0.5, 0.5, "100% NA", col = "red", cex = 1.5)
    } else {
      plot(dates[idx_stat], x_stat[idx_stat], type = "l",
           main = paste(s, "- Stationary"), xlab = "", ylab = "", col = "darkorange")
    }
  }
  
  mtext(paste(country_name, "-", type), outer = TRUE, cex = 1.2, line = 1)
  dev.off()
  cat("Plots saved in:", pdf_name, "\n")
}

plot_country_pdf <- function(country_data, country_stat, country_name) {
  plot_series_pdf(country_data$Revenues,     country_stat$Revenues,     country_name, "Revenues")
  plot_series_pdf(country_data$Expenditures, country_stat$Expenditures, country_name, "Expenditures")
}