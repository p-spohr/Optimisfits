# install packages

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(nlme)
library(tibble)
library(purrr)
library(splines)
library(lubridate)

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
mat_daily_log_returns = as.matrix(daily_log_returns[seq(2,ncol(daily_log_returns))])
head(mat_daily_log_returns)
current_portfolio
plot(daily_log_returns$Date, exp(cumsum(daily_log_returns$BMW)), type='l')
plot(daily_log_returns$Date, exp(cumsum(daily_log_returns$EUNL)), type='l')


# Calculate portfolio mean and standard deviation ####
port_log_ret = mat_daily_log_returns %*% current_portfolio$weight
plot(daily_log_returns$Date, exp(cumsum(port_log_ret)), type='l')
port_std = (t(current_portfolio$weight) %*% cov(mat_daily_log_returns) %*% current_portfolio$weight)^0.5 * sqrt(250)
ann_port = mean(port_log_ret) * 250
(ann_port - 0.02) / (port_std * sqrt(250))
lines(daily_log_returns$Date, exp(cumsum(port_log_ret)), type = 'l')

plot_returns = daily_log_returns %>%
  pivot_longer(
    cols = seq(2, ncol(daily_log_returns)),
    names_to = 'Ticker',
    values_to = 'LogReturns'
  )

# Original portfolio is highly correlated ####
cor(daily_log_returns[seq(2,ncol(daily_log_returns))])

ret_2024 = plot_returns %>%
  filter(Date >= ymd('2025-02-01'))

ggplot(ret_2024, aes(x=Date, y=50 * exp(cumsum(LogReturns)), color=Ticker)) +
  geom_line(linewidth=0.8) +
  labs(
    title='Log Returns',
    x='Date',
    y='Returns',
    color='Ticker'
  )
# Get Benchmark Data ####
head(port_log_ret)
head(tibble(Date = daily_log_returns$Date, LogReturns = port_log_ret[,1]))
portfolio_log_returns = tibble(Date = daily_log_returns$Date, LogReturns = port_log_ret[,1])
colnames(portfolio_log_returns) = c('Date', 'Portfolio')

dax_etf_prices = read.csv('dax_etf.csv')
head(dax_etf_prices)

dax_etf_prices$Date = date(ymd_hms(dax_etf_prices$Date))
dax_etf_prices = dax_etf_prices %>%
  select(Date, Close)
head(dax_etf_prices) 

dax_etf_log_ret = dax_etf_prices[-1,]
dax_etf_log_ret$Close = diff(log(dax_etf_prices$Close))
colnames(dax_etf_log_ret) = c('Date', 'DAX')
head(dax_etf_log_ret)  
head(portfolio_log_returns)

combined_port_bench = left_join(portfolio_log_returns, dax_etf_log_ret, by='Date')
head(combined_port_bench)
combined_port_bench = na.omit(combined_port_bench)
head(combined_port_bench)
head(combined_port_bench[c(2,3)])
cor(combined_port_bench[c(2,3)])
cor(combined_port_bench[2], combined_port_bench[2])

long_combined_port_bench = combined_port_bench %>%
  pivot_longer(
    cols = c(2,3),
    names_to = 'Portfolios',
    values_to = 'Returns'
  )

plot_port_bench = long_combined_port_bench %>%
  filter(Date >= '2025-01-01')

ggplot(plot_port_bench, aes(x=Date, y=50*exp(cumsum(Returns)), color=Portfolios)) +
  geom_line(linewidth=1) +
  labs(y='Log Returns',
       x='Date',
       color='Portfolios')

# Functions for getting yearly log returns (not necessary) ####
prepare_price_data_old = function(file_name, parent_dir) {
  
  # load csv into df
  hist_price = read.csv(file = paste0(parent_dir, '/', file_name)) %>%
    select(-3, -4, -5, -6, -7)
  colnames(hist_price) = c('Date', 'Price')
  hist_price$Date <- as.Date(hist_price$Date, format="%d.%m.%Y")
  hist_price[,2] = as.numeric(lapply(hist_price[,2], gsub, pattern = ',', replacement = '.'))
  
  # get log returns
  hist_price = hist_price[order(hist_price$Date),]
  logret = hist_price[-1,]
  logret[,2] = diff(log(hist_price[,2]))
  colnames(logret) = c('Date', 'Log Returns')
  
  # get yearly returns and standard deviation
  ret_sd = logret %>%
    mutate(year = floor_date(Date, 'year')) %>%
    group_by(year) %>%
    summarise(yearly = sum(`Log Returns`)) %>%
    summarise(yearly_return = mean(yearly), yearly_std = sd(yearly))
  
  return(ret_sd)
}

hist_price = read.csv(file = paste0('security_prices', '/', 'BMW.csv'))


get_yearly_returns = function(file_name, parent_dir) {
  
  # load csv into df
  hist_price = read.csv(file = paste0(parent_dir, '/', file_name)) %>%
    select(-3, -4, -5, -6, -7)
  colnames(hist_price) = c('Date', 'Price')
  hist_price$Date <- as.Date(hist_price$Date, format="%d.%m.%Y")
  hist_price[,2] = as.numeric(lapply(hist_price[,2], gsub, pattern = ',', replacement = '.'))
  
  # get log returns
  hist_price = hist_price[order(hist_price$Date),]
  logret = hist_price[-1,]
  logret[,2] = diff(log(hist_price[,2]))
  colnames(logret) = c('Date', 'Log Returns')
  
  # get yearly returns and standard deviation
  yearly_ret = logret %>%
    mutate(year = floor_date(Date, 'year')) %>%
    group_by(year) %>%
    summarise(yearly = sum(`Log Returns`))
  
  ticker = unlist(strsplit(file_name, '[.]'))[1]
  colnames(yearly_ret) = c('Date', ticker)
  
  return(yearly_ret)
}

weighted_portfolio_returns = get_yearly_returns('BMW.csv', 'security-prices')
colnames(weighted_portfolio_returns) = c('year', 'BMW')
weighted_portfolio_returns = 
get_yearly_returns('BMW.csv', 'security-prices')

for (file_name in sp_folder[-1]) {
  print(get_yearly_returns(file_name, 'security-prices'))
}

weighted_portfolio_returns = get_yearly_returns('BMW.csv', 'security-prices')

for (file_name in sp_folder[-1]) {
  weighted_portfolio_returns = left_join(weighted_portfolio_returns, get_yearly_returns(file_name, 'security-prices'), by='Date')
}
weighted_portfolio_returns[c(-1, -2),]
weighted_portfolio_returns
weighted_portfolio_returns = weighted_portfolio_returns[c(-1, -2),]
dim(weighted_portfolio_returns)
as.numeric(weighted_portfolio_returns[1, seq(2,8)]) * current_portfolio$weight
weighted_portfolio_returns[1, seq(2,8)]
mat_weighted_portfolio = matrix(ncol = 7, nrow = 9)

for(i in 1:nrow(weighted_portfolio_returns)) {
  
  new_weighted_returns = as.numeric(weighted_portfolio_returns[i, seq(2,8)]) * current_portfolio$weight
  
  print(new_weighted_returns)
  mat_weighted_portfolio[i,] = new_weighted_returns
}

mat_weighted_portfolio
total_port_ret = apply(mat_weighted_portfolio, 1, sum)
sum(mat_weighted_portfolio[1,])
plot(year(weighted_portfolio_returns$Date),total_port_ret*100, type='l')
year(weighted_portfolio_returns$Date)
length(weighted_portfolio_returns)
pivot_longer(weighted_portfolio_returns[1,], cols = )
weighted_portfolio_returns[1,] * current_portfolio$weight