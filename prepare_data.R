
# clean crypto prices ####
##########################
target_dir = 'raw_data_prices/crypto_prices'
csv_file_names = list.files(target_dir)
new_folder = 'crypto_prices_cleaned'

for(fp in csv_file_names) {
  df = read.csv(paste0(target_dir, '/', fp))
  df[[1]] = as.Date(df[[1]], format = '%m/%d/%Y')
  df[[2]] = as.numeric(lapply(df[[2]], gsub, pattern = ',', replacement = ''))
  write.csv(df, paste0(new_folder, '/', fp), row.names = FALSE)
}

# clean stock prices ####
##########################

target_dir = 'raw_data_prices/stock_prices'
csv_file_names = list.files(target_dir)
new_folder = 'stock_prices_cleaned'

csv_file_names = list.files(target_dir)

for(fp in csv_file_names) {
  df = read.csv(paste0(target_dir, '/', fp), sep = ';')
  df[[1]] = as.Date(df[[1]], format = '%d.%m.%Y')
  df[[2]] = as.numeric(lapply(df[[2]], gsub, pattern = ',', replacement = '.'))
  write.csv(df, paste0(new_folder, '/', fp), row.names = FALSE)
}

# clean etf prices ####
##########################

target_dir = 'raw_data_prices/etf_prices'
csv_file_names = list.files(target_dir)
new_folder = 'etf_prices_cleaned'

for(fp in csv_file_names) {
  df = read.csv(paste0(target_dir, '/', fp), sep = ';')
  df[[1]] = as.Date(df[[1]], format = '%d.%m.%Y')
  df[[2]] = as.numeric(lapply(df[[2]], gsub, pattern = ',', replacement = '.'))
  write.csv(df, paste0(new_folder, '/', fp), row.names = FALSE)
}

amem_prices = read.csv(paste0('etf_prices', '/', 'AMEM.csv'), sep = ';')
amem_prices[[2]]
head(amem_prices)


# clean metals prices ####
##########################

target_folder = 'raw_data_prices/metals_prices'
csv_file_names = list.files(target_folder)
new_folder = 'metals_prices_cleaned'

for(fp in csv_file_names) {
  df = read.csv(paste0(target_folder, '/', fp), sep = ';')
  df[[1]] = as.Date(df[[1]], format = '%d.%m.%Y')
  df[[2]] = as.numeric(lapply(df[[2]], gsub, pattern = ',', replacement = '.'))
  write.csv(df, paste0(new_folder, '/', fp), row.names = FALSE)
}

# clean original portfolio prices ####
##########################

target_folder = 'original_portfolio_prices'
csv_file_names = list.files(target_folder)
new_folder = 'original_portfolio_prices_cleaned'

for(fp in csv_file_names) {
  df = read.csv(paste0(target_folder, '/', fp), sep = ',')
  df[[1]] = as.Date(df[[1]], format = '%d.%m.%Y')
  df[[2]] = as.numeric(lapply(df[[2]], gsub, pattern = ',', replacement = '.'))
  write.csv(df, paste0(new_folder, '/', fp), row.names = FALSE)
}

# fix errors of missing data ####
#################################

# Check if csv files are available

for (ticker in target_portfolio$Ticker) {
result <- tryCatch(

  {
    read.csv(paste0('all_cleaned_prices_for_meta', '/', ticker, '.csv'))  # Code that might fail
  },
  error = function(e) {
    print(paste("Error:", ticker))  # Handle the error
    return(NULL)  # Return a fallback value
  })
}

for (ticker in target_portfolio$Ticker) {

  df = read.csv(paste0('all_cleaned_prices_for_meta', '/', ticker, '.csv'))
  df[[1]] = as.Date(df[[1]], format = '%Y-%m-%d')
  print(paste0(ticker,' Max: ', min(df[[1]])))

}

for (ticker in target_portfolio$Ticker) {

  df = read.csv(paste0('all_cleaned_prices_for_meta', '/', ticker, '.csv'))
  df[[1]] = as.Date(df[[1]], format = '%Y-%m-%d')
  print(paste0(ticker,' Max: ', sum(is.na(df[[2]]))))

}

for (ticker in target_portfolio$Ticker) {

  df = read.csv(paste0('all_cleaned_prices_for_meta', '/', ticker, '.csv'))
  df[[1]] = as.Date(df[[1]], format = '%Y-%m-%d')
  head(df)
  print(paste0(dim(df)))

}



