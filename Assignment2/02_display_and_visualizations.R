# BDA400 - Assignment 2
# Data display and visualizations
# Run this file from the Assignment2 folder after 01_setup_and_functions.R.

source("01_setup_and_functions.R")

# Load portfolio data and calculate the requested statistics.
stock_data <- load_stock_data("portfolio.txt",
                              from = Sys.Date() - 365,
                              to = Sys.Date())
statistics_table <- calculate_all_statistics(stock_data, ma_window = 20)

# Display a preview of each imported data frame.
for (symbol in names(stock_data)) {
  cat("\n========================================\n")
  cat("Stock:", symbol, "\n")
  cat("========================================\n")
  print(utils::head(stock_data[[symbol]], 10))
  cat("\nLast 10 observations:\n")
  print(utils::tail(stock_data[[symbol]], 10))
}

cat("\nSummary statistics for all available stocks:\n")
print(statistics_table)

# Save the numerical results and imported data.
write.csv(statistics_table, "stock_statistics.csv", row.names = FALSE)
for (symbol in names(stock_data)) {
  write.csv(stock_data[[symbol]],
            paste0("data_", symbol, ".csv"),
            row.names = FALSE)
}

# Combine adjusted closing prices for visualization.
price_data <- dplyr::bind_rows(lapply(names(stock_data), function(symbol) {
  df <- stock_data[[symbol]]
  data.frame(Date = df$Date,
             Adjusted = as.numeric(df$Adjusted),
             Symbol = symbol)
}))

price_plot <- ggplot(price_data,
                     aes(x = Date, y = Adjusted, color = Symbol)) +
  geom_line(linewidth = 0.6) +
  labs(title = "Adjusted Closing Prices",
       subtitle = "Daily stock prices downloaded from Yahoo Finance",
       x = "Date", y = "Adjusted closing price (USD)", color = "Stock") +
  theme_minimal()

print(price_plot)
ggsave("adjusted_closing_prices.png", price_plot,
       width = 10, height = 6, dpi = 300)

# Moving average plot for each stock.
for (symbol in names(stock_data)) {
  df <- stock_data[[symbol]]
  df$Adjusted <- as.numeric(df$Adjusted)
  df$Moving_Average_20 <- TTR::SMA(df$Adjusted, n = 20)

  ma_plot <- ggplot(df, aes(x = Date)) +
    geom_line(aes(y = Adjusted), linewidth = 0.5) +
    geom_line(aes(y = Moving_Average_20), linewidth = 0.8,
              na.rm = TRUE, linetype = "dashed") +
    labs(title = paste(symbol, "- Adjusted Close and 20-Day Moving Average"),
         x = "Date", y = "Price (USD)") +
    theme_minimal()

  print(ma_plot)
  ggsave(paste0("moving_average_", symbol, ".png"),
         ma_plot, width = 10, height = 6, dpi = 300)
}

cat("\nFiles created: stock_statistics.csv, data_*.csv, and PNG charts.\n")
