
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(lubridate)

sharpe_ratio = function(stock_returns, weights, rfr) {
  portfolio_return = mean(stock_returns %*% weights) * 250
  portfolio_std = as.numeric((t(weights) %*% cov(stock_returns) %*% weights)^0.5) * sqrt(250)
  print(paste0('mu: ', portfolio_return))
  print(paste0('sigma: ',portfolio_std))
  print(paste0('sharpe: ', (portfolio_return - rfr) / portfolio_std))
}
# Riskfree rate
RFR = log(1.0001) * 250


asset_log_returns = read.csv('portfolio_asset_log_returns.csv')
asset_log_returns = tibble(asset_log_returns)
asset_log_returns$Date = ymd(asset_log_returns$Date)

head(asset_log_returns)
dim(asset_log_returns)

# Max Sharpe Ratio Portfolio ####

max_sharpe_portfolio_meta = read.csv('max_sharpe_portfolio_stats/portfolio_max_sharpe_v7.csv')

max_sharpe_portfolio_meta = tibble(max_sharpe_portfolio_meta)

head(max_sharpe_portfolio_meta)

max_sharpe_portfolio_log_returns = as.matrix(asset_log_returns[-1]) %*% max_sharpe_portfolio_meta$Opt_Weights

dim(max_sharpe_portfolio_log_returns)

plot_max_sharpe_port_log_returns = exp(cumsum(max_sharpe_portfolio_log_returns))

plot(asset_log_returns$Date, plot_max_sharpe_port_log_returns, type = 'l')

# Suggested Portfolio Returns ####

suggested_portfolio_meta = read.csv('final_portfolio_stats/portfolio_final_weights_v7.csv')
suggested_portfolio_meta = tibble(suggested_portfolio_meta)

suggested_portfolio_meta

suggested_portfolio_log_returns = as.matrix(asset_log_returns[-1]) %*% suggested_portfolio_meta$Opt_Weights

plot_suggested_port_log_returns = exp(cumsum(suggested_portfolio_log_returns))

plot(asset_log_returns$Date, plot_suggested_port_log_returns, type = 'l')

sharpe_ratio(as.matrix(asset_log_returns[-1]), suggested_portfolio_meta$Opt_Weights, RFR)

# VaR of suggested portfolio and simulation

quantile(suggested_portfolio_log_returns, 0.95) * sqrt(250)

suggested_mu = -0.0217787
suggested_sigma = 0.1098528

N = 10000
S0 = 44.7
S1 = vector('numeric')
delta_t = 1
for (i in 1:N) {
  S1[i] = S0 * exp( (suggested_mu - 0.5 * suggested_sigma^2) * delta_t + sqrt(delta_t) * suggested_sigma * rnorm(1) )
}

# plot(S1, type = 'l')

quantile(-diff(log(S1/S0)), probs = 0.95)

# Plot suggested and max Sharpe portfolios ####

plot_shaded_returns = tibble(Date = asset_log_returns$Date, 
                             MaxSharpe = plot_max_sharpe_port_log_returns, 
                             Suggested = plot_suggested_port_log_returns)
head(plot_shaded_returns)

ggplot(plot_shaded_returns, aes(x = Date)) +
  geom_ribbon(aes(ymin = Suggested, 
                  ymax = MaxSharpe), 
              fill = "orange", alpha = 0.5) +
  labs(title = "One Euro Invested ",
       x = "Years", y = "Euro") +
  theme_minimal() +
  theme(plot.title = element_text(size = 22, hjust = 0.5),
        axis.title = element_text(size = 17),
        axis.text = element_text(size = 14))

# Value at risk of suggested and max Sharpe portfolios ####

# Select the current portfolio ####

target_port_csv = 'portfolio_final_weights_v7.csv'
port_meta = read.csv(paste0(getwd(), '/', 'portfolio_metas', '/', target_port_csv))
head(port_meta)

# filter based on current security choices 
target_portfolio = port_meta %>%
  filter(Port == 1)

# number of securities in current portfolio
n_stocks = length(target_portfolio$Ticker)



allocation = c(4.9, 9.8, 3.5, 6.6, 8.4, 11.5)
sum(allocation)
sum(allocation) + 2.9 + 2.8
weights = allocation / sum(allocation)
sum(weights)
target_portfolio$Opt_Weights = weights
target_portfolio
sharpe_ratio(stock_returns = mat_daily_log_returns, rfr = RFR, weights = target_portfolio$Opt_Weights)


original_mu = 0.022159672
original_sigma = 0.172406086
original_sharpe = -0.016467388
N = 1000
delta_t = 1/N

# Geometric brownian motion full path ####
S0 = 1
S1 = vector('numeric')
S1[1] = S0
for (i in 2:N) {
  S1[i] = S1[i-1] * exp( (original_mu - 0.5 * original_sigma^2) * delta_t + sqrt(delta_t) * original_sigma * rnorm(1) )
}

plot(S1, type = 'l')
  
# Geometric brownian motion delta 1 ####
# yearly t = 1
N = 10000
S0 = 44.7
S1 = vector('numeric')
delta_t = 1
for (i in 1:N) {
  S1[i] = S0 * exp( (original_mu - 0.5 * original_sigma^2) * delta_t + sqrt(delta_t) * original_sigma * rnorm(1) )
}

# plot(S1, type = 'l')

quantile(-diff(log(S1/S0)), probs = 0.95)
# Value at Risk (VaR) at the 95% confidence level is 0.04 (or 4%), 
# this means that there is a 95% chance that the potential loss will not exceed 4% of the portfolio's value 
# over the specified time period. However, there is still a 5% chance that the loss could be greater than 4%.
hist(-diff(log(S1/S0)))

VaR_port = quantile(-(mat_daily_log_returns %*% weights), probs = 0.95)

as.numeric(VaR_port) * sqrt(250)

hist(-(mat_daily_log_returns %*% weights), breaks = 20)

plot(exp(cumsum(mat_daily_log_returns %*% weights)), type = 'l')



