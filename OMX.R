
################################################################################
#                             Empirical Data                                   #
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

omx2020_2024 <- read_excel("C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/omx2020_2024.xlsx", 
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
                                         "numeric", "skip", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "skip", "numeric", "numeric", "numeric", 
                                         "skip", "numeric", "skip", "numeric", 
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
                                         "skip", "skip", "numeric", "numeric", 
                                         "skip", "numeric", "numeric", "numeric", 
                                         "skip", "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "skip", "numeric", "numeric", 
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
                                         "numeric", "numeric", "skip", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "skip", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "skip", "numeric", "numeric", 
                                         "numeric", "skip", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "skip", "numeric", "numeric", 
                                         "numeric", "numeric", "skip", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "skip", "numeric", 
                                         "skip", "numeric", "numeric", "numeric", 
                                         "numeric", "skip", "numeric", "numeric", 
                                         "numeric", "numeric", "skip", "numeric", 
                                         "skip", "skip", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "skip", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "skip", "numeric", "numeric", 
                                         "skip", "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "skip", "skip", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "skip", "numeric", "skip", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "skip", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "skip", "numeric", "numeric", 
                                         "skip", "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "skip", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "skip", "skip", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "skip", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "skip", "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "skip", "numeric", "numeric", "numeric", 
                                         "numeric", "skip", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "skip", "numeric", "numeric", "numeric", 
                                         "skip", "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "skip", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric"))

stibor2020_2024 <- read_excel("C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/stibor_2020_2024.xlsx", 
                              col_types = c("date", "numeric"))

stibor <- xts(stibor2020_2024[, -1], order.by = stibor2020_2024[[1]])/100 # Convert to decimals
omx <-as.matrix(xts(omx2020_2024[,-1], order.by = omx2020_2024[[1]]))

########################################
#   Weekly and monthly rebalancing    #
########################################

# Log returns and risk free rate
returns <- diff(log(omx)) # omx contains daily prices
risk_free <- as.numeric(log(1 + stibor/252))[-nrow(stibor)] # risk free in decimals, 252 business days

# Data set includes "röda dagar" which need to be removed
# Find indices of rows where all elements are zero
zero_rows <- which(apply(returns, 1, function(x) all(x == 0)))

# Remove "röda dagar"
returns <- returns[-zero_rows,]
risk_free <- risk_free[-zero_rows]

###############################
# p=50
# Select 50 random stocks
#random50 <- sample(1:347, 50)
#returns50 <- as.matrix(returns[, c(random50)])
#saveRDS(returns50, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/returns_used_in_analysis_50.rds")
returns50 <- readRDS("C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/returns_used_in_analysis_50.rds")

start.time <- Sys.time()
rolling_window_results_month_2021_2024 <- mega_rol_pred_parallel(returns50, 252, 21, rf=risk_free, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_month_2021_2024$stats
saveRDS(rolling_window_results_month_2021_2024, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/updated_results/rolling_window_results_month_2021_2024_50.rds")

start.time <- Sys.time()
rolling_window_results_week_2021_2024 <- mega_rol_pred_parallel(returns50, 252, 5, rf=risk_free, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_week_2021_2024$stats
saveRDS(rolling_window_results_week_2021_2024, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/updated_results/rolling_window_results_week_2021_2024_50.rds")


################################################################################
# p=150

# Select 150 random stocks
#random150 <- sample(1:347, 150)
#returns150 <- as.matrix(returns[, c(random150)])
#saveRDS(returns150, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/updated_results/returns_used_in_analysis_150.rds")
returns150 <- readRDS("C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/returns_used_in_analysis_150.rds")

start.time <- Sys.time()
rolling_window_results_month_2021_2024_150 <- mega_rol_pred_parallel(returns150, 252, 21, rf=risk_free, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_month_2021_2024_150$stats
saveRDS(rolling_window_results_month_2021_2024_150, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/updated_results/rolling_window_results_month_2021_2024_150.rds")


start.time <- Sys.time()
rolling_window_results_week_2021_2024_150 <- mega_rol_pred_parallel(returns150, 252, 5, rf=risk_free, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_week_2021_2024_150$stats
saveRDS(rolling_window_results_week_2021_2024_150, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/updated_results/rolling_window_results_week_2021_2024_150.rds")


################################################################################
# p=250

# Select 250 random stocks
#random250 <- sample(1:347, 250)
#returns250 <- as.matrix(returns[, c(random250)])
#saveRDS(returns250, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/updated_results/returns_used_in_analysis_250.rds")
returns250 <- readRDS("C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/returns_used_in_analysis_250.rds")

start.time <- Sys.time()
rolling_window_results_month_2021_2024_250 <- mega_rol_pred_parallel(returns250, 252, 21, rf=risk_free, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_month_2021_2024_250$stats
saveRDS(rolling_window_results_month_2021_2024_250, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/updated_results/rolling_window_results_month_2021_2024_250.rds")


start.time <- Sys.time()
rolling_window_results_week_2021_2024_250 <- mega_rol_pred_parallel(returns250, 252, 5, rf=risk_free, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_week_2021_2024_250$stats
saveRDS(rolling_window_results_week_2021_2024_250, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/updated_results/rolling_window_results_week_2021_2024_250.rds")


##############################
#     Daily rebalancing      #
##############################
# Load earlier data sets
returns50 <- readRDS("C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/returns_used_in_analysis_50.rds")
returns150 <- readRDS("C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/returns_used_in_analysis_150.rds")
returns250 <- readRDS("C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/returns_used_in_analysis_250.rds")

start.time <- Sys.time()
rolling_window_results_daily_2021_2024_50 <- mega_rol_pred_parallel(returns50[505:1008,], 252, 1, rf=risk_free[505:1008], max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_daily_2021_2024_50$stats
saveRDS(rolling_window_results_daily_2021_2024_50, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/updated_results/rolling_window_results_daily_2021_2024_50.rds")


start.time <- Sys.time()
rolling_window_results_daily_2021_2024_150 <- mega_rol_pred_parallel(returns150[505:1008,], 252, 1, rf=risk_free[505:1008], max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_daily_2021_2024_150$stats
saveRDS(rolling_window_results_daily_2021_2024_150, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/updated_results/rolling_window_results_daily_2021_2024_150.rds")


start.time <- Sys.time()
rolling_window_results_daily_2021_2024_250 <- mega_rol_pred_parallel(returns250[505:1008,], 252, 1, rf=risk_free[505:1008], max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_daily_2021_2024_250$stats
saveRDS(rolling_window_results_daily_2021_2024_250, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/updated_results/rolling_window_results_daily_2021_2024_250.rds")



