
all_parent_dir = 'all_securities'
path_to_all = paste0(getwd(), '/', all_parent_dir)
all_security_csv = list.files(path_to_all)

target_port_csv = 'example_portfolio_meta.csv'
port_meta = read.csv(paste0(getwd(), '/', target_port_csv))

n_stocks = length(port_meta$Ticker)

price_to_log_returns = function(parent_dir, file_name) {
  
  rel_path = paste0(parent_dir, '/', file_name)
  
  
  # check separator in file
  first_line = readLines(paste0(parent_dir, '/', file_name), n=1)
  separator = ''
  if(grepl(';', first_line)) {
    separator = ';'
  } else {
    separator = ','
  }
  
  # return(print(separator))
  
  hist_price = read.csv(file = rel_path, sep = separator) %>%
    select(1,2)
  
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
for(ticker in port_meta$Ticker) {
  f_name = paste0(ticker, '.', 'csv' )
  log_ret = price_to_log_returns(all_parent_dir, f_name)
  daily_log_returns = left_join(daily_log_returns, log_ret, by='Date')
  
}

daily_log_returns = na.omit(daily_log_returns)
head(daily_log_returns)

opt_sharpe_ratio_p = function(par, stock_returns, rfr) {
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
stock_n = ncol(daily_log_returns) - 1
initial_values_p = c(rep(1/stock_n,stock_n))
lower_b_p = port_meta$Target - 0.05
upper_b_p = port_meta$Target + 0.05
RFR = log(1.0001)*250


mat_daily_log_returns = as.matrix(daily_log_returns[-1])

head(mat_daily_log_returns)

optimum_weights = optim(initial_values_p, 
                        opt_sharpe_ratio_p, 
                        stock_returns = mat_daily_log_returns, 
                        rfr = RFR, 
                        method = 'L-BFGS-B',
                        lower = lower_b_p,
                        upper = upper_b_p)
#control = list(fnscale=-1) another way to maximize besides negating function

print(optimum_weights$par)
optimum_weights
sum(optimum_weights$par)



# combine optimal weights with ticker
current_portfolio$optimal_weights = as.numeric(optimum_weights$par)
print(current_portfolio)

sharpe_ratio = function(stock_returns, weights, rfr) {
  portfolio_return = mean(stock_returns %*% weights) * 250
  portfolio_std = as.numeric((t(weights) %*% cov(stock_returns) %*% weights)^0.5) * sqrt(250)
  print(portfolio_return)
  print(portfolio_std)
  print((portfolio_return - rfr) / portfolio_std)
}

sharpe_ratio(mat_daily_log_returns, current_portfolio$optimal_weights, RFR)

port_meta$optimum_weights = optimum_weights$par

print(port_meta)

