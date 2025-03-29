# install.packages('corrplot')
library(corrplot)

head(daily_log_returns)

portfolio_corr = cor(as.matrix(daily_log_returns[-1]))

col_gradient = colorRampPalette(c('blue', 'lightblue', 'white', 'pink', 'red'))(200)
corrplot(portfolio_corr, method = 'color', tl.col = 'black', tl.cex = 0.85, cl.cex = 0.85, tl.srt= 45, type = 'lower', col = col_gradient)


head(target_portfolio)

class_ticker = target_portfolio %>%
  group_by(Class) %>%
  select(Ticker) %>%
  print(n=100)

?corrplot