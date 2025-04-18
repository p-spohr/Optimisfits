
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(nlme)
library(tibble)
library(purrr)
library(splines)
library(lubridate)


library(mvtnorm)

# fPortfolio
library(fPortfolio)
library(timeSeries)
# Sharpe Ratio ####
stock_n = 5
days_n = 250
log_returns = matrix(rnorm(stock_n * days_n, mean = 0.002, sd = 0.05), ncol = stock_n, nrow =days_n)

sharpe_ratio = function(stock_returns, weights, rfr) {
  portfolio_return = mean(stock_returns %*% weights) * 250
  portfolio_std = as.numeric((t(weights) %*% cov(stock_returns) %*% weights)^0.5) * sqrt(250)
  print(portfolio_return)
  print(portfolio_std)
  print((portfolio_return - rfr) / portfolio_std)
}
RFR = log(1.002)*250
sharpe_ratio(log_returns, rep(1/stock_n, stock_n), RFR)

# Optimize Weights ####

?optim

initial_values = c(rep(1/stock_n,stock_n))
lower_b = c(0.01, 0, 0, 0, 0)
upper_b = c(1, 1, 1, 1, 0.5)

opt_sharpe_ratio = function(par, stock_returns, rfr) {
  # print(par)
  portfolio_return = mean(stock_returns %*% par) * 250
  # print(portfolio_return)
  portfolio_std = as.numeric((t(par) %*% cov(stock_returns) %*% par)^0.5) * sqrt(250)
  # print(portfolio_std)
  sharp_r = (portfolio_return - rfr) / portfolio_std
  # if (sum(par) > 1 | sum(par) < 1) {
  #   sharp_r = sharp_r + 100
  # }
  return(-sharp_r)
}

opt_weights = optim(initial_values, 
      opt_sharpe_ratio, 
      stock_returns = log_returns, 
      rfr = RFR, 
      method = 'L-BFGS-B',
      lower = lower_b,
      upper = upper_b)

opt_weights$par / sum(opt_weights$par)
sharpe_ratio(log_returns, opt_weights$par / sum(opt_weights$par), RFR)

initial_values_w = c(1, rep(1/stock_n,stock_n))
lower_b_w = c(-Inf, 0, 0, 0, 0, 0)
upper_b_w = c(Inf, 1, 1, 1, 1, 1)

opt_sharpe_ratio_w = function(par, stock_returns, rfr) {
  print(par)
  portfolio_return = mean(stock_returns %*% par[2:6]) * 250
  # print(portfolio_return)
  portfolio_std = as.numeric((t(par[2:6]) %*% cov(stock_returns) %*% par[2:6])^0.5) * sqrt(250)
  # print(portfolio_std)
  sharp_r = (portfolio_return - rfr) / portfolio_std
  print(sharp_r)
  # par[1] = sum(par[2:6])
  one_check = sum(par[2:6])
  if(one_check > 1) {
    penalty = 100 * sum(par[2:6])
  } else {
    penalty = 0
  }
  return(-sharp_r + penalty)
}

optim(initial_values_w, 
      opt_sharpe_ratio_w, 
      stock_returns = log_returns, 
      rfr = RFR, 
      method = 'L-BFGS-B',
      lower = lower_b_w,
      upper = upper_b_w)



initial_values_p = c(rep(1/stock_n,stock_n))
lower_b_p = c(0.05, 0.1, 0, 0, 0)
upper_b_p = c(1, 1, 1, 1, 1)

# ITS WORKING !!! ####
opt_sharpe_ratio_p = function(par, stock_returns, rfr) {
  print(par)
  portfolio_return = mean(stock_returns %*% par) * 250
  # print(portfolio_return)
  portfolio_std = as.numeric((t(par) %*% cov(stock_returns) %*% par)^0.5) * sqrt(250)
  # print(portfolio_std)
  sharp_r = (portfolio_return - rfr) / portfolio_std
  print(sharp_r)
  one_check = sum(par)
  if(one_check != 1) {
    penalty = 10000 * (1 - one_check)^2
  } else {
    penalty = 0
  }
  return(-(sharp_r - penalty))
}

optimum_weights = optim(initial_values_p, 
      opt_sharpe_ratio_p, 
      stock_returns = log_returns, 
      rfr = RFR, 
      method = 'L-BFGS-B',
      lower = lower_b_p,
      upper = upper_b_p)
      #control = list(fnscale=-1) another way to maximize besides negating function

sum(optimum_weights$par)



# mvtnorm ####

mu = c(0.001, 0.003, 0.002)
sigma = matrix(c(0.03, 0.015, 0.005, 
                 0.015, 0.004, 0.002, 
                 0.005, 0.002, 0.035), ncol = 3)
eigen(sigma)
log_returns_corr = rmvnorm(250, c(0.001, 0.003, 0.002), sigma)

sharpe_ratio(log_returns_corr, rep(1/3, 3), log(1.001)*250)

?optim
log(1.001)*250
cov(log_returns)

?as.timeSeries

# Using fPortfolio ####
# install.packages('fPortfolio')
# install.packages("timeSeries")  # Install the package

# Example usage
data <- matrix(rnorm(100), ncol = 5)  # Create example data
time_series_data <- as.timeSeries(data)  # Convert to timeSeries object
head(time_series_data)

# Portfolio specifications
spec <- portfolioSpec()
setRiskFreeRate(spec) <- 0.01  # Set risk-free rate

# Constraints
constraints <- "LongOnly"

# Calculate efficient frontier
frontier <- portfolioFrontier(time_series_data, spec, constraints)

# Plot the efficient frontier
plot(frontier, c(1,2,3,4))


summary(frontier)
getWeights(frontier)
getData(frontier)
getStatistics(frontier)
frontier@portfolio
targ_ret = getTargetReturn(frontier)
targ_risk = getTargetRisk(frontier)
plot(targ_risk[,2], targ_ret[,1], type='l')

# Set up current portfolio meta information ####
tickers = c('BMW', 'EUNL', 'EXS1', 'SXRI', 'MBG', 'SXR8')
file_names = sp_folder = list.files(paste0(getwd(), '/security_prices'))
allocation = c(8.4, 3.5, 9.8, 4.9, 11.5, 6.6)
current_portfolio = data.frame(security = tickers, account = allocation, file_name = file_names)
current_portfolio = current_portfolio %>%
  mutate(weight = account / sum(account))
current_portfolio
sum(current_portfolio$weight)

getwd()
setwd('C:\\Users\\pat_h\\OneDrive\\p-spohr-repos\\Optimisfits')

# Get log returns ####

price_to_log_returns = function(parent_dir, file_name) {
  rel_path = paste0(parent_dir, '/', file_name)
  hist_price = read.csv(file = rel_path) %>%
    select(-3, -4, -5, -6, -7)
  colnames(hist_price) = c('Date', 'Price')
  hist_price$Date <- as.Date(hist_price$Date, format="%d.%m.%Y")
  hist_price[,2] = as.numeric(lapply(hist_price[,2], gsub, pattern = ',', replacement = '.'))
  
  # get log returns
  ticker = unlist(strsplit(file_name, split = '[.]'))[1]
  hist_price = hist_price[order(hist_price$Date),]
  log_ret = hist_price[-1,]
  log_ret[,2] = diff(log(hist_price[,2]))
  colnames(log_ret) = c('Date', ticker)
  return(log_ret)
  
}

daily_log_returns = tibble(Date = seq(ymd('2017-01-01'), ymd('2026-01-01'), by='day'))
head(daily_log_returns)
for(f_name in current_portfolio$file_name) {
  log_ret = price_to_log_returns('security_prices', f_name)
  daily_log_returns = left_join(daily_log_returns, log_ret, by='Date')
  
}

daily_log_returns = na.omit(daily_log_returns)
head(daily_log_returns)

# Transform
time_series_data <- as.timeSeries(daily_log_returns)  # Convert to timeSeries object
head(time_series_data)

procentage_daily_returns = time_series_data * 100
head(procentage_daily_returns)

# Portfolio specifications
spec <- portfolioSpec()
setOptimize(spec) <- 'maxSharpeRatio'
print(spec)
setop
setRiskFreeRate(spec) <- 0.01  # Set risk-free rate

# Constraints
constraints <- "LongOnly"

# Calculate efficient frontier
frontier <- portfolioFrontier(procentage_daily_returns, spec, constraints)

# Plot the efficient frontier
plot(frontier, c(1,2,3,4))

optimum_weights = getWeights(frontier)
optimum_weights
getPortfolio(frontier)
frontier@data

portfolioSpec(frontier)
