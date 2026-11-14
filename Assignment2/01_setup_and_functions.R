# BDA400 - Assignment 2
# Technical Analysis using R - Preliminary Stage
# Student: Lujain Ibrahim

required_packages <- c("quantmod", "TTR", "ggplot2", "dplyr", "knitr")

install_if_missing <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
}

install_if_missing(required_packages)

suppressPackageStartupMessages({
  library(quantmod)
  library(TTR)
  library(ggplot2)
  library(dplyr)
})

# Read one stock symbol per line from portfolio.txt.
read_portfolio <- function(file = "portfolio.txt") {
  if (!file.exists(file)) {
    stop("Cannot find portfolio file: ", file)
  }
  symbols <- trimws(readLines(file, warn = FALSE))
  symbols <- symbols[nzchar(symbols)]
  symbols <- unique(symbols)
  if (length(symbols) == 0) stop("portfolio.txt contains no stock symbols.")
  symbols
}

# Import daily stock prices from Yahoo Finance.
load_stock_data <- function(file = "portfolio.txt",
                            from = Sys.Date() - 365,
                            to = Sys.Date()) {
  symbols <- read_portfolio(file)
  stock_data <- list()
  failed_symbols <- character(0)

  for (symbol in symbols) {
    message("Downloading: ", symbol)
    result <- tryCatch(
      getSymbols(symbol, src = "yahoo", from = from, to = to,
                 auto.assign = FALSE, warnings = FALSE),
      error = function(e) {
        message("Could not download ", symbol, ": ", conditionMessage(e))
        NULL
      }
    )

    if (is.null(result)) {
      failed_symbols <- c(failed_symbols, symbol)
    } else {
      stock_data[[symbol]] <- as.data.frame(result)
      # quantmod prefixes xts column names with the ticker (for example AAPL.Adjusted).
      # Remove that prefix so every stock data frame has consistent column names.
      names(stock_data[[symbol]]) <- sub("^.*\\.", "", names(stock_data[[symbol]]))
      stock_data[[symbol]]$Date <- as.Date(rownames(stock_data[[symbol]]))
      rownames(stock_data[[symbol]]) <- NULL
      stock_data[[symbol]] <- stock_data[[symbol]][
        , c("Date", setdiff(names(stock_data[[symbol]]), "Date")),
        drop = FALSE
      ]
    }
  }

  if (length(stock_data) == 0) {
    stop("No stock data was downloaded. Check the symbols and internet connection.")
  }
  if (length(failed_symbols) > 0) {
    warning("No data returned for: ", paste(failed_symbols, collapse = ", "))
  }

  stock_data
}

# Statistical mode: the most frequent observed value.
get_mode <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  values <- unique(x)
  values[which.max(tabulate(match(x, values)))]
}

# Calculate requested statistics using Adjusted Close prices.
calculate_statistics <- function(stock_df, symbol = "Unknown", ma_window = 20) {
  required_col <- "Adjusted"
  if (!required_col %in% names(stock_df)) {
    stop("Adjusted closing-price column is missing for ", symbol)
  }

  prices <- as.numeric(stock_df[[required_col]])
  prices <- prices[is.finite(prices)]
  if (length(prices) == 0) stop("No valid adjusted prices for ", symbol)

  moving_average <- TTR::SMA(prices, n = min(ma_window, length(prices)))
  data.frame(
    Symbol = symbol,
    Observations = length(prices),
    Latest_Adjusted_Close = tail(prices, 1),
    Moving_Average_20 = tail(moving_average[is.finite(moving_average)], 1),
    Mean = mean(prices),
    Mode = get_mode(prices),
    Median = median(prices),
    Standard_Deviation = sd(prices),
    row.names = NULL
  )
}

calculate_all_statistics <- function(stock_data, ma_window = 20) {
  results <- lapply(names(stock_data), function(symbol) {
    calculate_statistics(stock_data[[symbol]], symbol, ma_window)
  })
  dplyr::bind_rows(results)
}
