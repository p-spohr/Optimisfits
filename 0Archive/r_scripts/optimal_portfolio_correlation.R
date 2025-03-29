# install.packages('corrplot')
library(corrplot)

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

original_portfolio_csv = list.files('original_portfolio_prices')
tickers = vector('character')
i = 1
for(fp in original_portfolio_csv){
  tickers[i] = unlist(strsplit(fp, split = '[.]'))[1]
  i = i + 1
}

daily_log_returns = tibble(Date = seq(ymd('2017-01-01'), ymd('2026-01-01'), by='day'))
head(daily_log_returns)
for(ticker in tickers) {
  f_name = paste0(ticker, '.', 'csv' )
  log_ret = price_to_log_returns('original_portfolio_prices', f_name)
  daily_log_returns = left_join(daily_log_returns, log_ret, by='Date')
  
}

daily_log_returns = na.omit(daily_log_returns)
head(daily_log_returns)

original_portfolio_corr = cor(as.matrix(daily_log_returns[-1]))

col_gradient = colorRampPalette(c('blue', 'lightblue', 'white', 'pink', 'red'))(200)
corrplot(original_portfolio_corr, method = 'color', tl.col = 'black', col = col_gradient, type = 'lower')
COL2(diverging = c("RdBu", "BrBG", "PiYG", "PRGn", "PuOr", "RdYlBu"), n = 200)

?COL2
?corrplot



