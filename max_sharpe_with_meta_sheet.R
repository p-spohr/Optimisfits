
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(lubridate)

# getwd()

# Get all price csv files available ####

all_parent_dir = 'all_cleaned_prices_for_meta'
path_to_all = paste0(getwd(), '/', all_parent_dir)
all_security_csv = list.files(path_to_all)

# Select the current portfolio ####

target_port_csv = 'portfolio_v7.csv'
port_meta = read.csv(paste0(getwd(), '/', 'portfolio_metas', '/', target_port_csv))
head(port_meta)

# filter based on current security choices 
target_portfolio = port_meta %>%
  filter(Port == 1)

# number of securities in current portfolio
n_stocks = length(target_portfolio$Ticker)

# Prepare log returns for each security ####

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

daily_log_returns = tibble(Date = seq(ymd('2022-01-01'), ymd('2025-04-01'), by='day'))
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

# plot(daily_log_returns$Date, exp(cumsum(daily_log_returns[['DIA']])), type = 'l')

head(daily_log_returns)
dim(daily_log_returns)
min(daily_log_returns$Date)

# Basic portfolio stats ####


# as.matrix(daily_log_returns[-1])
# allocation = c(4.9, 9.8, 3.5, 6.6, 8.4, 11.5)
# sum(allocation) + 2.9 + 2.8
# weights = allocation / sum(allocation)
# sum(weights)
# mean(as.matrix(daily_log_returns[-1]) %*% weights) * 250
# as.numeric((t(weights) %*% cov(as.matrix(daily_log_returns[-1])) %*% weights)^0.5) * sqrt(250)
# 
# sharpe_ratio(as.matrix(daily_log_returns[-1]), weights = weights, rfr = RFR)

#as.matrix(daily_log_returns[-1Sh)arpe ratio function to pass into optimizer ####

# Maximize Sharpe ratio ####

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

# initial weights
initial_weights = c(rep(1/n_stocks, n_stocks))

# lower and upper bounds of the weights
# target_portfolio$Target = rep(0.5, n_stocks)

# lower_b = target_portfolio$Target - 0.01
lower_b = rep(0, n_stocks)

# upper_b = target_portfolio$Target + 0.01
upper_b = rep(1, n_stocks)

# Riskfree rate
RFR = log(1.0001) * 250


mat_daily_log_returns = as.matrix(daily_log_returns[-1])

write.csv(daily_log_returns, file = 'portfolio_log_returns.csv', row.names = FALSE)

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


# set weights in target portfolio
target_portfolio$Opt_Weights = optimum_weights$par

write.csv(target_portfolio, 'portfolio_max_sharpe_v7.csv', row.names = FALSE)


target_portfolio %>%
  select(Name, Ticker, Opt_Weights) %>%
  arrange(desc(Opt_Weights))

sector_final = target_portfolio %>%
  group_by(Sector) %>%
  summarise(sum(Opt_Weights)) %>%
  arrange(desc(`sum(Opt_Weights)`))
sector_final

# write.csv(sector_final, 'sector_final.csv', row.names = FALSE)

class_final = target_portfolio %>%
  group_by(Class) %>%
  summarise(sum(Opt_Weights)) %>%
  arrange(desc(`sum(Opt_Weights)`))
class_final

# write.csv(class_final, 'class_final.csv', row.names = FALSE)

geo_final = target_portfolio %>%
  group_by(Geo.Zone) %>%
  summarise(sum(Opt_Weights)) %>%
  arrange(desc(`sum(Opt_Weights)`))
geo_final

write.csv(geo_final, 'geo_final.csv', row.names = FALSE)

head(daily_log_returns)
daily_log_returns[c('BTC-USD', 'LTC-USD', 'ETH-USD')] %>%
  summarise(across(everything(), sd)) %>%
  pivot_longer(cols = everything(), names_to = 'Ticker', values_to = 'StandardDeviation') %>%
  mutate(YearlySD = StandardDeviation * sqrt(250))

target_portfolio %>%
  select(Name, Class, Opt_Weights) %>%
  filter(Class == 'ETF') %>%
  arrange(desc(Opt_Weights))

daily_log_returns[-1] %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(cols = everything(), names_to = 'Ticker', values_to = 'Returns') %>%
  print(n=100)

plot(daily_log_returns$Date, exp(cumsum(daily_log_returns[['SXRI']])), type = 'l')

for(ticker in target_portfolio$Ticker) {
  plot(daily_log_returns$Date, exp(cumsum(daily_log_returns[[ticker]])), type = 'l')
}

daily_log_returns[['XOM']]

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

sharpe_ratio(stock_returns = mat_daily_log_returns, rfr = RFR, weights = target_portfolio$Opt_Weights)

