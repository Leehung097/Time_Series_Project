#==============================
#    Portfolio Backtesting
#==============================

#--------------------
#    Prepare data
#--------------------

# Clear workspace: 
rm(list = ls())


# Collect price data from Yahoo: 
symbols <- c("FB", "AMZN", "NFLX", "GOOG", "^IXIC") # Note that ^IXIC is NASDAQ Composite.  
tidyquant::tq_get(x = symbols, from = "2016-01-01", to = "2021-08-10") -> data_raw

# Convert to wide form: 

library(tidyverse)

data_raw %>% 
  select(date, symbol, adjusted) %>% 
  pivot_wider(names_from = symbol, values_from = adjusted) -> data_wide_price


date_ymd <- data_wide_price$date # Date time series. 

market_data <- data_wide_price$`^IXIC` # Assume that IXIC is market indicator. 

stocks_data <- data_wide_price %>% select(symbols[1:4]) # Historical price data for FB, AMZN NFLX and GOOG. 

n <- ncol(stocks_data) # Number of stocks. 

# Convert to xts object: 

library(xts)

xts(stocks_data, order.by = date_ymd) -> stocks_data_xts
xts(market_data, order.by = date_ymd) -> market_data_xts

# Prepare data for backtesting: 
data_for_backtesting <- list(list(adjusted = stocks_data_xts, index = market_data_xts))

#----------------------------------------------------------------------------
#  Compare two portfolios by Portfolio Backtesting Process (Naive approach)
#----------------------------------------------------------------------------

# Equal-weighted portfolio: 

portfolio1 <- function(...) {
  
  equal_weights <- rep(1 / n, n)
  
  return(equal_weights)
}

# Portfolio contains 40% FB, 10% AMZN, 25% NFLX, 25% GOOG: 

portfolio2 <- function(...) {
  
  weights <- c(0.4, 0.1, 0.25, 0.25)
  
  return(weights)
  
}

# Back-testing process for comparing: 

portfolios <- list("Portfolio1" = portfolio1, "Portfolio2" = portfolio2)

library(portfolioBacktest)

bt <- portfolioBacktest(portfolio_funs = portfolios, 
                        data = data_for_backtesting, 
                        benchmark = c("index"), 
                        T_rolling_window = 252*4, 
                        show_progress_bar = TRUE)

res_sum <- backtestSummary(bt)

res_sum$performance_summary[1:9, 1:2] %>% 
  data.frame() %>% 
  mutate(Metric = row.names(.)) %>% 
  mutate_if(is.numeric, function(x) {round(x, 3)}) %>% 
  select(Metric, everything()) -> df_compare

rownames(df_compare) <- NULL

library(kableExtra) # For presenting table. 

# Cpmpare performance: 

df_compare %>% 
  kbl(caption = "Table 1: Portfolio Performance", escape = TRUE) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria")
