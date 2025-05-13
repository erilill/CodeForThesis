
################################################################################
library(readxl)
library(parallel)
library(PortfolioMoments)
library(corpcor)
library(POET)
library(glasso)
library(PerformanceAnalytics)
library(xts)
library(zoo)
library(TVMVP)
source("mega_rol_pred.R")
sp500 <- sp500_2021_2024 <- read_excel("C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/sp500_2021_2024.xlsx", 
                                       col_types = c("date", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "skip", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "skip", "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "skip", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "skip", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "skip", "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "skip", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "skip", "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric"))

sp500 <-as.matrix(xts(sp500[,-1], order.by = sp500[[1]]))

library(dplyr)
library(lubridate)
library(tidyr)

# Step 1: Import the Excel sheet
dat <- read_excel("C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/tbill_2021_2024.xlsx")

# Step 2: Clean the data (convert rates to numeric)
colnames(dat) <- c("Date", "Rate")
dat$Rate <- as.numeric(gsub(",", ".", dat$Rate)) / 100  # Convert rates to decimal

# Step 3: Create a sequence of all daily dates
full_dates <- seq.Date(from = as.Date(min(dat$Date)), 
                       to = as.Date(max(dat$Date)), 
                       by = "day")

# Step 4: Create a data frame with full date range
daily_data <- data.frame(Date = full_dates)

# Step 5: Merge the original monthly data to the daily data
# Fill in missing dates with the previous month's rate
daily_data <- daily_data %>%
  left_join(dat, by = "Date") %>%
  tidyr::fill(Rate, .direction = "down")

# Step 6: Compute daily rates (Assuming 252 trading days per year)
daily_data <- daily_data %>%
  mutate(Daily_Log_Rate = log(1 + Rate / 252))
daily_data <- daily_data[!weekdays(full_dates) %in% c("Saturday", "Sunday"),]
risk_free <- as.numeric(daily_data[,3])

# Log returns and risk free rate
returns <- diff(log(sp500)) # omx contains daily prices

# Data set includes "röda dagar" which need to be removed
# Find indices of rows where all elements are zero
zero_rows <- which(apply(returns, 1, function(x) all(x == 0)))

# Remove "röda dagar"
returns <- returns[-zero_rows,]
risk_free <- risk_free[-zero_rows]

########################################
#   Weekly and monthly rebalancing    #
########################################
# p=50
# Select 50 random stocks
random50 <- sample(1:496, 50)
returns50 <- as.matrix(returns[, c(random50)])
saveRDS(returns50, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/returns_used_in_analysis_50_sp500.rds")

start.time <- Sys.time()
rolling_window_results_month_2021_2024_sp500 <- mega_rol_pred_parallel(returns50, 252, 21, rf=risk_free, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_month_2021_2024_sp500$stats
saveRDS(rolling_window_results_month_2021_2024_sp500, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/rolling_window_results_month_2021_2024_sp500.rds")

start.time <- Sys.time()
rolling_window_results_week_2021_2024_sp500 <- mega_rol_pred_parallel(returns50, 252, 5, rf=risk_free, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_week_2021_2024_sp500$stats
saveRDS(rolling_window_results_week_2021_2024_sp500, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/rolling_window_results_week_2021_2024_sp500.rds")


################################################################################
# p=150

# Select 150 random stocks
random150 <- sample(1:496, 150)
returns150 <- as.matrix(returns[, c(random150)])
saveRDS(returns150, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/returns_used_in_analysis_150_sp500.rds")

start.time <- Sys.time()
rolling_window_results_month_2021_2024_150_sp500 <- mega_rol_pred_parallel(returns150, 252, 21, rf=risk_free, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_month_2021_2024_150_sp500$stats
saveRDS(rolling_window_results_month_2021_2024_150_sp500, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/rolling_window_results_month_2021_2024_150_sp500.rds")


start.time <- Sys.time()
rolling_window_results_week_2021_2024_150_sp500 <- mega_rol_pred_parallel(returns150, 252, 5, rf=risk_free, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_week_2021_2024_150_sp500$stats
saveRDS(rolling_window_results_week_2021_2024_150_sp500, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/rolling_window_results_week_2021_2024_150_sp500.rds")


################################################################################
# p=250

# Select 250 random stocks
random250 <- sample(1:496, 250)
returns250 <- as.matrix(returns[, c(random250)])
saveRDS(returns250, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/returns_used_in_analysis_250_sp500.rds")

start.time <- Sys.time()
rolling_window_results_month_2021_2024_250_sp500 <- mega_rol_pred_parallel(returns250, 252, 21, rf=risk_free, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_month_2021_2024_250_sp500$stats
saveRDS(rolling_window_results_month_2021_2024_250_sp500, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/rolling_window_results_month_2021_2024_250_sp500.rds")


start.time <- Sys.time()
rolling_window_results_week_2021_2024_250_sp500 <- mega_rol_pred_parallel(returns250, 252, 5, rf=risk_free, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_week_2021_2024_250_sp500$stats
saveRDS(rolling_window_results_week_2021_2024_250_sp500, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/rolling_window_results_week_2021_2024_250_sp500.rds")


##############################
#     Daily rebalancing      #
##############################
# Load earlier data sets
returns50 <- readRDS("C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/returns_used_in_analysis_50_sp500.rds")
returns150 <- readRDS("C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/returns_used_in_analysis_150_sp500.rds")
returns250 <- readRDS("C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/returns_used_in_analysis_250_sp500.rds")

start.time <- Sys.time()
rolling_window_results_daily_2021_2024_50_sp500 <- mega_rol_pred_parallel(returns50, 252, 1, rf=risk_free, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_daily_2021_2024_50_sp500$stats
saveRDS(rolling_window_results_daily_2021_2024_50_sp500, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/rolling_window_results_daily_2021_2024_50_sp500.rds")


start.time <- Sys.time()
rolling_window_results_daily_2021_2024_150_sp500 <- mega_rol_pred_parallel(returns150, 252, 1, rf=risk_free, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_daily_2021_2024_150_sp500$stats
saveRDS(rolling_window_results_daily_2021_2024_150_sp500, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/rolling_window_results_daily_2021_2024_150_sp500.rds")


start.time <- Sys.time()
rolling_window_results_daily_2021_2024_250_sp500 <- mega_rol_pred_parallel(returns250, 252, 1, rf=risk_free, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_daily_2021_2024_250_sp500$stats
saveRDS(rolling_window_results_daily_2021_2024_250_sp500, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/rolling_window_results_daily_2021_2024_250_sp500.rds")

