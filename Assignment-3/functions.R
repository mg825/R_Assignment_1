library(zoo)
library(xts)

moving_avg <- function(prices, window = 50){
  vec <- prices * NA
  
  for (i in window:length(prices)) {
    vec[i] <- sum(prices[(i - window + 1):i]) / window
  }
  return(vec)
}


f_fast_MA <- function(xts_prices, window = 50, FUN = mean, ...) {
  rollapplyr(xts_prices, width = window, FUN = FUN, fill = NA, align = "right")
}

f_slow_MA <- function(xts_prices, window = 200, FUN = mean, ...) {
  rollapplyr(xts_prices, width = window, FUN = FUN, fill = NA, align = "right")
}

signals <- function(fast, slow) {
  # Generate signal: 1 = buy, -1 = sell, 0 = hold
  sig <- ifelse(fast > slow, 1,
                ifelse(fast < slow, -1, 0))
  xts(sig, order.by = index(fast))
}

backtest <- function(xts_prices, xts_signals) {
  
  returns <- xts_prices / stats::lag(xts_prices) - 1
  strategyReturns <- stats::lag(xts_signals) * returns
  
  returns <- na.fill(returns, 0)
  strategyReturns <- na.fill(strategyReturns, 0)
  
  names(returns) <- "Benchmark"
  names(strategyReturns) <- "Strategy"
  
  compareReturns <- cbind(returns, strategyReturns)
  return(compareReturns)
}

strategyReturns <- function(xts_prices, xts_signals) {
  returns <- xts_prices / stats::lag(xts_prices) - 1
  strat_returns <- stats::lag(xts_signals) * returns

  strat_returns <- na.fill(strat_returns, 0)

  total_profit <- prod(1 + strat_returns, na.rm = TRUE) - 1
  
  return(as.numeric(total_profit))
}