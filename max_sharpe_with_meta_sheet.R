
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(lubridate)

getwd()

# Get all price csv files available ####
###############################

all_parent_dir = 'all_cleaned_prices_for_meta'
path_to_all = paste0(getwd(), '/', all_parent_dir)
all_security_csv = list.files(path_to_all)

# Select the current portfolio ####
###################################

target_port_csv = 'portfolio_base.csv'
port_meta = read.csv(paste0(getwd(), '/', 'portfolio_metas', '/', target_port_csv))
head(port_meta)

# filter based on current security choices 
target_portfolio = port_meta %>%
  filter(Port == 1)

# number of securities in current portfolio
n_stocks = length(target_portfolio$Ticker)

# Prepare log returns for each security ####
############################################
price_to_log_returns = function(parent_dir, file_name) {
  
  rel_path = paste0(parent_dir, '/', file_name)
  
  hist_price = read.csv(file = rel_path, sep = ',') %>%
    select(1,2)
  
  colnames(hist_price) = c('Date', 'Price')
  
  hist_price$Date <- as.Date(hist_price$Date, format="%Y-%m-%d")
  
  # get log returns
  ticker = unlist(strsplit(file_name, split = '[.]'))[1]
  hist_price = hist_price[order(hist_price$Date),]
  log_ret = hist_price[-1,]
  log_ret[,2] = diff(log(hist_price[,2]))
  
  colnames(log_ret) = c('Date', ticker)
  
  return(log_ret)
  
}

# Join the log returns into one data frame #####
daily_log_returns = tibble(Date = seq(ymd('2017-01-01'), ymd('2026-01-01'), by='day'))
head(daily_log_returns)
for(ticker in target_portfolio$Ticker) {
  f_name = paste0(ticker, '.', 'csv' )
  
  # get log returns for security
  log_ret = price_to_log_returns(all_parent_dir, f_name)
  
  # join the security's log returns along Date column
  daily_log_returns = left_join(daily_log_returns, log_ret, by='Date')
  
}

# remove NA to keep data uniform
daily_log_returns = na.omit(daily_log_returns)
head(daily_log_returns)
dim(daily_log_returns)

# Sharpe ratio function to pass into optimizer ####
###################################################

opt_sharpe_ratio = function(par, stock_returns, rfr) {
  # print(par)
  # yearly portfolio return
  portfolio_return = mean(stock_returns %*% par) * 250
  # print(portfolio_return)
  portfolio_std = as.numeric((t(par) %*% cov(stock_returns) %*% par)^0.5) * sqrt(250)
  # print(portfolio_std)
  sharp_r = (portfolio_return - rfr) / portfolio_std
  print(sharp_r)
  
  # penalize when the sum of weights not equal to one
  one_check = sum(par)
  if(one_check != 1) {
    # penalty should be parabolic
    penalty = 10000 * (1 - one_check)^2
  } else {
    penalty = 0
  }
  # negate sharpe ratio because it is a minimization algorithm
  # the negated function's minimum is its maximum
  return(-(sharp_r - penalty))
}

# Optimize the current portfolio by maximizing the Sharpe ratio ####
####################################################################

# initial weights
initial_weights = c(rep(1/n_stocks, n_stocks))

# lower and upper bounds of the weights
target_portfolio$Target = rep(0.5, n_stocks)
lower_b = target_portfolio$Target - 0.4
upper_b = target_portfolio$Target + 0.20

# Riskfree rate
RFR = log(1.0001)*250


mat_daily_log_returns = as.matrix(daily_log_returns[-1])

head(mat_daily_log_returns)

optimum_weights = optim(initial_weights, 
                        opt_sharpe_ratio, 
                        stock_returns = mat_daily_log_returns, 
                        rfr = RFR, 
                        method = 'L-BFGS-B',
                        lower = lower_b,
                        upper = upper_b)
#control = list(fnscale=-1) another way to maximize besides negating function

print(optimum_weights$par)
sum(optimum_weights$par)



# combine optimal weights with ticker
current_portfolio$optimal_weights = as.numeric(optimum_weights$par)
print(current_portfolio)

sharpe_ratio = function(stock_returns, weights, rfr) {
  portfolio_return = mean(stock_returns %*% weights) * 250
  portfolio_std = as.numeric((t(weights) %*% cov(stock_returns) %*% weights)^0.5) * sqrt(250)
  print(paste0('mu: ', portfolio_return))
  print(paste0('sigma: ',portfolio_std))
  print(paste0('sharpe: ', (portfolio_return - rfr) / portfolio_std))
}

sharpe_ratio(mat_daily_log_returns, current_portfolio$optimal_weights, RFR)

port_meta$optimum_weights = optimum_weights$par

print(port_meta)
write.csv(port_meta, file = 'example_mixed_portfolio_opti_weights.csv', row.names = FALSE)

# Optimize without target weight bounds
stock_n = ncol(daily_log_returns) - 1
initial_values_p = c(rep(1/stock_n,stock_n))
lower_b = rep(0, stock_n)
upper_b = rep(1, stock_n)
RFR = log(1.0001)*250


mat_daily_log_returns = as.matrix(daily_log_returns[-1])

head(mat_daily_log_returns)

optimum_weights_unbounded = optim(initial_values_p, 
                        opt_sharpe_ratio_p, 
                        stock_returns = mat_daily_log_returns, 
                        rfr = RFR, 
                        method = 'L-BFGS-B',
                        lower = lower_b,
                        upper = upper_b)

port_meta$optimum_weights = optimum_weights_unbounded$par
port_meta
write.csv(port_meta, file = 'example_mixed_portfolio_opti_weights_unbounded.csv', row.names = FALSE)

barplot_meta = port_meta %>%
  filter(optimum_weights > 0)


barplot(barplot_meta$optimum_weights, names.arg = barplot_meta$Ticker)

sharpe_ratio(stock_returns = mat_daily_log_returns, rfr = RFR, weights = port_meta$optimum_weights)
