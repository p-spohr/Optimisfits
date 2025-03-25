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

setwd('C:\\Users\\pat_h\\OneDrive\\p-spohr-repos\\Optimisfits')
list.files(paste0(getwd(), '/security-prices'))

# Get Prices In Data Frame ####

prepare_price_data = function(path_to_file) {
  hist_price = read.csv(file = path_to_file) %>%
    select(-3, -4, -5, -6, -7)
  colnames(hist_price) = c('Date', 'Price')
  hist_price$Date <- as.Date(hist_price$Date, format="%d.%m.%Y")
  hist_price[,2] = as.numeric(lapply(hist_price[,2], gsub, pattern = ',', replacement = '.'))
  return(hist_price)
}
csv_file_names = vector()
csv_file_names = list.files(paste0(getwd(), '/security-prices'))
head(bmw_hist)
bmw_hist = prepare_price_data(paste0('security-prices/', csv_file_names[1]))
bmw_hist = bmw_hist[order(bmw_hist$Date),]
diff(log(bmw_hist[,2]))
mean(diff(log(bmw_hist[,2])))
bmw_hist[1,]
head(bmw_hist)
ym(bmw_hist[1,1])
head(bmw_hist)
bmw_logret = bmw_hist[-1,]
bmw_logret[,2] = diff(log(bmw_hist[,2]))
colnames(bmw_logret) = c('Date', 'Log Returns')
head(bmw_logret)
bmw_logret %>%
  mutate(month = floor_date(Date, 'month')) %>%
  group_by('month') %>%
  summarise(total = mean(`Log Returns`))

head(bmw_logret)

bmw_logret %>%
  mutate(year = floor_date(Date, 'year')) %>%
  group_by('year') %>%
  summarise(total = sum(`Log Returns`))

?floor_date
bmw_logret %>%
  mutate(month = floor_date(Date, 'month')) %>%
  group_by(month) %>%
  summarise(monthly = sum(`Log Returns`))

bmw_logret %>%
  mutate(year = floor_date(Date, 'year')) %>%
  group_by(year) %>%
  summarise(yearly = sum(`Log Returns`))

bmw_logret %>%
  mutate(year = floor_date(Date, 'year')) %>%
  group_by(year) %>%
  summarise(yearly = sum(`Log Returns`)) %>%
  summarise(yearly_return = mean(yearly), yearly_std = sd(yearly))
?sd
bmw_logret %>%
  mutate(month = floor_date(Date, 'month')) %>%
  group_by(month) %>%
  summarise(montly = sum(`Log Returns`)) %>%
  summarise(montly_return = mean(montly), montly_std = sd(montly))

0.0805 *sqrt(8)

sd(bmw_logret[,2]) * sqrt(20)

# Individually ####

#BMW
bmw_hist <- read.csv(file = "security-prices/BMWG Cronologia Dati.csv") %>%
  select(-3, -4, -5, -6, -7)
head(bmw_hist)
colnames(bmw_hist) = c('Date', 'Price')
bmw_hist$Date <- as.Date(bmw_hist$Date, format="%d.%m.%Y")

bmw_hist[,2] = as.numeric(lapply(bmw_hist[,2], gsub, pattern = ',', replacement = '.'))

ggplot(bmw_hist, aes(Date, Price)) + 
  geom_line()
  

#MBGn
MBGn_hist <- read.csv(file = "Current Portfolio/Data/MBGn Cronologia Dati.csv") %>%
  select(-3, -4, -5, -6, -7)
MBGn_hist$Data <- as.Date(MBGn_hist$Data, format="%d.%m.%Y")
MBGn_hist <- MBGn_hist %>%
  column_to_rownames("Data")
colnames(MBGn_hist) <- c("Price")



#EUNL
EUNL_hist <- read.csv(file = "Current Portfolio/Data/Storico EUNL.csv") %>%
  select(-3, -4, -5, -6, -7)
EUNL_hist$Data <- as.Date(EUNL_hist$Data, format="%d.%m.%Y")
EUNL_hist <- EUNL_hist %>%
  column_to_rownames("Data")
colnames(EUNL_hist) <- c("Price")

#GDAXIEX
EUNL_hist <- read.csv(file = "Current Portfolio/Data/Storico EUNL.csv") %>%
  select(-3, -4, -5, -6, -7)
EUNL_hist$Data <- as.Date(EUNL_hist$Data, format="%d.%m.%Y")
EUNL_hist <- EUNL_hist %>%
  column_to_rownames("Data")
colnames(EUNL_hist) <- c("Price")

#BUND
BUND_hist <- read_xls("Current Portfolio/Data/BUND TF 0% MG36 EU_historical_price.xls") %>%
  select(-2, -3, -4, -5, -7, -8, -9, -10)
BUND_hist$Date <- as.Date(BUND_hist$Date, format="%d.%m.%Y")
BUND_hist <- BUND_hist %>%
  column_to_rownames("Date")
colnames(BUND_hist) <- c("Price")

#IBCI
IBCI_hist <- read.csv(file = "Current Portfolio/Data/Storico IBCI.csv") %>%
  select(-3, -4, -5, -6, -7)
IBCI_hist$Data <- as.Date(IBCI_hist$Data, format="%d.%m.%Y")
IBCI_hist <- IBCI_hist %>%
  column_to_rownames("Data")
colnames(IBCI_hist) <- c("Price")

#ISHIGH
ISHIGH_hist <- read.csv(file = "Current Portfolio/Data/Storico ISHIGH.csv") %>%
  select(-3, -4, -5, -6, -7)
ISHIGH_hist$Data <- as.Date(ISHIGH_hist$Data, format="%d.%m.%Y")
ISHIGH_hist <- ISHIGH_hist %>%
  column_to_rownames("Data")
colnames(ISHIGH_hist) <- c("Price")

#SXR8
SXR8_hist <- read.csv(file = "Current Portfolio/Data/Storico SXR8.csv") %>%
  select(-3, -4, -5, -6, -7)
SXR8_hist$Data <- as.Date(SXR8_hist$Data, format="%d.%m.%Y")
SXR8_hist <- SXR8_hist %>%
  column_to_rownames("Data")
colnames(SXR8_hist) <- c("Price")

