# 2016-2019 OMX data
# Wanted to try pre-pandemic/war market performance
library(readxl)
library(parallel)
library(PortfolioMoments)
library(corpcor)
library(POET)
library(glasso)
library(PerformanceAnalytics)
library(xts)
library(zoo)
source("utils.R")
source("mega_rol_pred.R")

omx_15_19 <- read_excel("C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/omx_15_19.xlsx", 
                        col_types = c("date", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "skip", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "skip", "numeric", "numeric", "skip", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "skip", "numeric", "skip", "numeric", 
                                      "skip", "numeric", "numeric", "numeric", 
                                      "numeric", "skip", "skip", "skip", 
                                      "skip", "skip", "numeric", "skip", 
                                      "numeric", "numeric", "skip", "skip", 
                                      "skip", "skip", "skip", "numeric", 
                                      "numeric", "numeric", "skip", "numeric", 
                                      "skip", "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "skip", "skip", "skip", "numeric", 
                                      "skip", "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "skip", "skip", 
                                      "numeric", "skip", "numeric", "numeric", 
                                      "skip", "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "skip", "numeric", 
                                      "numeric", "skip", "numeric", "numeric", 
                                      "skip", "skip", "skip", "numeric", 
                                      "numeric", "skip", "numeric", "numeric", 
                                      "numeric", "skip", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "skip", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "skip", "numeric", "numeric", "numeric", 
                                      "skip", "skip", "numeric", "skip", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "skip", "numeric", 
                                      "numeric", "numeric", "skip", "numeric", 
                                      "numeric", "skip", "numeric", "numeric", 
                                      "skip", "numeric", "skip", "skip", 
                                      "numeric", "numeric", "numeric", 
                                      "skip", "skip", "numeric", "numeric", 
                                      "skip", "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "skip", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "skip", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "skip", "skip", "skip", "skip", "numeric", 
                                      "numeric", "skip", "skip", "numeric", 
                                      "numeric", "skip", "skip", "numeric", 
                                      "skip", "skip", "numeric", "numeric", 
                                      "skip", "numeric", "numeric", "numeric", 
                                      "skip", "numeric", "numeric", "numeric", 
                                      "numeric", "skip", "numeric", "numeric", 
                                      "numeric", "skip", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "skip", "skip", "numeric", "numeric", 
                                      "numeric", "skip", "skip", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "skip", "numeric", "numeric", "numeric", 
                                      "skip", "numeric", "skip", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "skip", "numeric", "numeric", "skip", 
                                      "skip", "skip", "skip", "skip", "numeric", 
                                      "numeric", "numeric", "skip", "numeric", 
                                      "skip", "numeric", "numeric", "skip", 
                                      "numeric", "numeric", "skip", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "skip", "numeric", 
                                      "numeric", "skip", "skip", "skip", 
                                      "skip", "numeric", "numeric", "numeric", 
                                      "numeric", "skip", "skip", "numeric", 
                                      "skip", "skip", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "skip", "numeric", "skip", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "skip", "numeric", "skip", "numeric", 
                                      "numeric", "numeric", "skip", "numeric", 
                                      "numeric", "skip", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "skip", "numeric", "skip", "skip", 
                                      "numeric", "numeric", "skip", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "skip", "numeric", "numeric", "skip", 
                                      "numeric", "skip", "numeric", "numeric", 
                                      "numeric", "skip", "skip", "skip", 
                                      "numeric", "skip", "numeric", "numeric", 
                                      "numeric", "skip", "skip", "numeric", 
                                      "skip", "numeric", "numeric", "numeric", 
                                      "numeric", "skip", "skip", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "skip", "skip", "skip", "numeric", 
                                      "numeric", "skip", "numeric", "skip", 
                                      "skip", "numeric", "skip", "numeric", 
                                      "skip", "skip", "skip", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "skip", "numeric", "skip", "numeric", 
                                      "skip", "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "skip", "skip", 
                                      "skip", "skip", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "skip", "numeric"))
stibor_15_19 <- read_excel("C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/stibor_15_19.xlsx", 
                           col_types = c("date", "numeric"))

stibor_15_19 <- xts(stibor_15_19[, -1], order.by = stibor_15_19[[1]])/100 # Convert to decimals
omx_15_19 <-as.matrix(xts(omx_15_19[,-1], order.by = omx_15_19[[1]]))

# Log returns and risk free rate
returns_15_19 <- diff(log(omx_15_19)) # omx contains daily prices
risk_free_15_19 <- as.numeric(log(1 + stibor_15_19/252))[-nrow(stibor_15_19)] # risk free in decimals, 252 business days

# Data set includes "röda dagar" which need to be removed
# Find indices of rows where all elements are zero
zero_rows_15_19 <- which(apply(returns_15_19, 1, function(x) all(x == 0)))

# Remove "röda dagar"
returns_15_19 <- returns_15_19[-zero_rows_15_19,]
risk_free_15_19 <- risk_free_15_19[-zero_rows_15_19]


###############################
# p=50
# Select 50 random stocks
random50 <- sample(1:261, 50)
returns_15_19_50 <- as.matrix(returns_15_19[, c(random50)])
saveRDS(returns_15_19_50, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/returns_15_19_50.rds")

start.time <- Sys.time()
rolling_window_results_month_15_19 <- mega_rol_pred_parallel(returns_15_19_50, 252, 21, rf=risk_free_15_19, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_month_15_19$stats
saveRDS(rolling_window_results_month_15_19, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/rolling_window_results_month_15_19.rds")

start.time <- Sys.time()
rolling_window_results_week_15_19 <- mega_rol_pred_parallel(returns_15_19_50, 252, 5, rf=risk_free_15_19, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_week_15_19$stats
saveRDS(rolling_window_results_week_15_19, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/rolling_window_results_week_15_19.rds")


################################################################################
# p=150

# Select 150 random stocks
random150 <- sample(1:347, 150)
returns150 <- as.matrix(returns[, c(random150)])
saveRDS(returns150, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/returns_used_in_analysis_150.rds")

start.time <- Sys.time()
rolling_window_results_month_2021_2024_150 <- mega_rol_pred_parallel(returns150, 252, 21, rf=risk_free, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_month_2021_2024_150$stats
saveRDS(rolling_window_results_month_2021_2024_150, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/rolling_window_results_month_2021_2024_150.rds")


start.time <- Sys.time()
rolling_window_results_week_2021_2024_150 <- mega_rol_pred_parallel(returns150, 252, 5, rf=risk_free, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_week_2021_2024_150$stats
saveRDS(rolling_window_results_week_2021_2024_150, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/rolling_window_results_week_2021_2024_150.rds")


################################################################################
# p=250

# Select 250 random stocks
random250 <- sample(1:347, 250)
returns250 <- as.matrix(returns[, c(random250)])
saveRDS(returns250, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/returns_used_in_analysis_250.rds")

start.time <- Sys.time()
rolling_window_results_month_2021_2024_250 <- mega_rol_pred_parallel(returns250, 252, 21, rf=risk_free, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_month_2021_2024_250$stats
saveRDS(rolling_window_results_week_2021_2024_250, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/rolling_window_results_week_2021_2024_250.rds")


start.time <- Sys.time()
rolling_window_results_week_2021_2024_250 <- mega_rol_pred_parallel(returns250, 252, 5, rf=risk_free, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_week_2021_2024_250$stats
saveRDS(rolling_window_results_week_2021_2024_250, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/rolling_window_results_week_2021_2024_250.rds")


