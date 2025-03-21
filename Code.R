library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(nlme)
library(tibble)
library(purrr)
library(splines)

#BMW
bmw_hist <- read.csv(file = "Current Portfolio/Data/BMWG Cronologia Dati.csv") %>%
  select(-3, -4, -5, -6, -7)
bmw_hist$Data <- as.Date(bmw_hist$Data, format="%d.%m.%Y")
bmw_hist <- bmw_hist %>%
  column_to_rownames("Data")
colnames(bmw_hist) <- c("Price")

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

