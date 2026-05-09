plot_nowcast_vs_realized <- function(nowcast_df, ragged_df, country_name = "Country") {
  
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  
  # ---------------------------------------------------------
  # 1. Prepare the realized quarterly series
  # ---------------------------------------------------------
  realized_series <- data.frame(
    Date = as.Date(ragged_df[[1]]),
    Value = as.numeric(ragged_df[[2]])
  )
  
  realized_series <- realized_series %>%
    filter(!is.na(Value))
  
  realized_quarterly <- realized_series %>%
    filter(month(Date) %in% c(3, 6, 9, 12)) %>%
    mutate(
      Realized_Standardized = as.numeric(scale(Value)),
      Quarter_Date = as.Date(paste0(year(Date), "-", sprintf("%02d", month(Date)), "-01"))
    )
  
  # ---------------------------------------------------------
  # 2. Prepare the nowcast data
  # ---------------------------------------------------------
  nowcast_plot <- nowcast_df %>%
    mutate(
      Publication_Date = as.Date(publication_date),
      Quarter_Date = case_when(
        month(Publication_Date) %in% 1:3   ~ as.Date(paste0(year(Publication_Date), "-03-01")),
        month(Publication_Date) %in% 4:6   ~ as.Date(paste0(year(Publication_Date), "-06-01")),
        month(Publication_Date) %in% 7:9   ~ as.Date(paste0(year(Publication_Date), "-09-01")),
        month(Publication_Date) %in% 10:12 ~ as.Date(paste0(year(Publication_Date), "-12-01"))
      )
    ) %>%
    arrange(Publication_Date)
  
  # ---------------------------------------------------------
  # 3. Merge nowcast and realized data
  # ---------------------------------------------------------
  df_final <- nowcast_plot %>%
    left_join(
      realized_quarterly %>% select(Quarter_Date, Realized_Standardized),
      by = "Quarter_Date"
    )
  
  # ---------------------------------------------------------
  # 4. Plot
  # ---------------------------------------------------------
  my_colors <- c(
    "Realized"     = "black",
    "factor_midas" = "#e74c3c",
    "sg_lasso"     = "#3498db",
    "ar1"          = "#2ecc71",
    "rw"           = "#f39c12"
  )
  
  p <- ggplot(df_final, aes(x = Publication_Date)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.7) +
    
    geom_step(
      aes(y = Realized_Standardized, color = "Realized"),
      linewidth = 1,
      linetype = "dashed"
    ) +
    
    geom_line(
      aes(y = nowcast_value, color = model),
      linewidth = 0.8,
      alpha = 0.8
    ) +
    
    scale_color_manual(values = my_colors) +
    
    theme_minimal() +
    labs(
      title = paste("Nowcast vs Realized:", country_name),
      subtitle = "Pseudo Real-Time Forecast",
      x = "Publication Date",
      y = "Standardized Value",
      color = "Legend"
    ) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 14)
    )
  
  return(p)
}
