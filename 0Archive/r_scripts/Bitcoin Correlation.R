library(tidyverse)
library(readxl)    
library(lubridate) 

bitcoin <- read.csv(file = "Bitcoin Historical Data.csv") %>%
  select(-3, -4, -5, -6, -7)
bitcoin$Date <- as.Date(bitcoin$Date, format="%m/%d/%Y")
bitcoin$Price <- as.numeric(gsub("\\,", "", bitcoin$Price))
bitcoin <- bitcoin %>%
  column_to_rownames("Date")
colnames(bitcoin) <- c("Price")

atx <- read_xlsx("Indexes Data/ATX.xlsx") %>%
  select(-2, -3, -4, -6, -7, -8, -9, -10)
atx$Date <- as.Date(atx$Date, format="%m/%d/%Y")
atx <- atx %>%
  column_to_rownames("Date")
colnames(atx) <- c("Price")

dax <- read_xlsx("Indexes Data/DAX.xlsx") %>%
  select(-2, -3, -4, -6, -7, -8, -9, -10)
dax$Date <- as.Date(dax$Date, format="%m/%d/%Y")
dax <- dax %>%
  column_to_rownames("Date")
colnames(dax) <- c("Price")

nasdaq <- read_xlsx("Indexes Data/NASDAQ.xlsx") %>%
  select(-2, -3, -4, -6, -7, -8, -9, -10)
nasdaq$Date <- as.Date(nasdaq$Date, format="%m/%d/%Y")
nasdaq <- nasdaq %>%
  column_to_rownames("Date")
colnames(nasdaq) <- c("Price")

sp500 <- read_xlsx("Indexes Data/SP500.xlsx") %>%
  select(-2, -3, -4, -6, -7, -8, -9, -10)
sp500$Date <- as.Date(sp500$Date, format="%m/%d/%Y")
sp500 <- sp500 %>%
  column_to_rownames("Date")
colnames(sp500) <- c("Price")

calcola_rendimenti <- function(df) {
  df <- df %>% mutate(Returns = log(Price / lag(Price))) %>%
  select(-Price)
  return(na.omit(df))
}

# Applica la funzione ai dataset
bitcoin <- calcola_rendimenti(bitcoin)
atx <- calcola_rendimenti(atx)
dax <- calcola_rendimenti(dax)
nasdaq <- calcola_rendimenti(nasdaq)
sp500 <- calcola_rendimenti(sp500)

bitcoin <- bitcoin %>% rownames_to_column("Date")
atx <- atx %>% rownames_to_column("Date")
dax <- dax %>% rownames_to_column("Date")
nasdaq <- nasdaq %>% rownames_to_column("Date")
sp500 <- sp500 %>% rownames_to_column("Date")

merged_data <- reduce(list(bitcoin, atx, dax, nasdaq, sp500), full_join, by = "Date")

# Rename return columns
colnames(merged_data) <- c("Date", "Bitcoin", "ATX", "DAX", "NASDAQ", "SP500")

# Convert Date back to Date type
merged_data$Date <- as.Date(merged_data$Date)

# Remove rows with any missing values
merged_data <- na.omit(merged_data)

# Function to compute correlation for a given period
calculate_correlation <- function(data, start_date, end_date) {
  filtered_data <- filter(data, Date >= as.Date(start_date) & Date <= as.Date(end_date))
  return(cor(filtered_data[-1]))  # Remove Date column before correlation calculation
}

# Compute correlations
cor_all_time <- cor(merged_data[-1])  # Exclude Date column
cor_2016_2020 <- calculate_correlation(merged_data, "2016-01-01", "2020-12-31")
cor_2020_2024 <- calculate_correlation(merged_data, "2020-01-01", "2024-12-31")
cor_2024_now <- calculate_correlation(merged_data, "2024-01-01", Sys.Date())

cor_all_time <- cor_all_time["Bitcoin",]
cor_2016_2020 <- cor_2016_2020["Bitcoin", ]
cor_2020_2024 <- cor_2020_2024["Bitcoin", ]
cor_2024_now <- cor_2024_now["Bitcoin", ]

# Print results
print("All-time correlation")
print(cor_all_time)

print("Correlation 2016-2020")
print(cor_2016_2020)

print("Correlation 2020-2024")
print(cor_2020_2024)

print("Correlation 2024-Now")
print(cor_2024_now)
