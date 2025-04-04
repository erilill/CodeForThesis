#############################
# Maximum Sharpe

# Load earlier data sets
returns50 <- readRDS("C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/returns_used_in_analysis_50.rds")
returns150 <- readRDS("C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/returns_used_in_analysis_150.rds")
returns250 <- readRDS("C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/returns_used_in_analysis_250.rds")

########################################
#   Weekly and monthly rebalancing    #
########################################
# p=50

start.time <- Sys.time()
rolling_window_results_month_2021_2024_sr <- mega_rol_pred_parallel_maxsharpe_all(returns50, 252, 21, rf=risk_free, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_month_2021_2024_sr$stats
saveRDS(rolling_window_results_month_2021_2024_sr, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/rolling_window_results_month_2021_2024_sr.rds")

start.time <- Sys.time()
rolling_window_results_week_2021_2024_sr <- mega_rol_pred_parallel_maxsharpe_all(returns50, 252, 5, rf=risk_free, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_week_2021_2024_sr$stats
saveRDS(rolling_window_results_week_2021_2024_sr, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/rolling_window_results_week_2021_2024_sr.rds")


################################################################################
# p=150

start.time <- Sys.time()
rolling_window_results_month_2021_2024_150_sr <- mega_rol_pred_parallel_maxsharpe_all(returns150, 252, 21, rf=risk_free, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_month_2021_2024_150_sr$stats
saveRDS(rolling_window_results_month_2021_2024_150_sr, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/rolling_window_results_month_2021_2024_150_sr.rds")


start.time <- Sys.time()
rolling_window_results_week_2021_2024_150_sr <- mega_rol_pred_parallel_maxsharpe_all(returns150, 252, 5, rf=risk_free, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_week_2021_2024_150_sr$stats
saveRDS(rolling_window_results_week_2021_2024_150_sr, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/rolling_window_results_week_2021_2024_150_sr.rds")


################################################################################
# p=250

start.time <- Sys.time()
rolling_window_results_month_2021_2024_250_sr <- mega_rol_pred_parallel_maxsharpe_all(returns250, 252, 21, rf=risk_free, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_month_2021_2024_250_sr$stats
saveRDS(rolling_window_results_month_2021_2024_250_sr, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/rolling_window_results_month_2021_2024_250_sr.rds")


start.time <- Sys.time()
rolling_window_results_week_2021_2024_250_sr <- mega_rol_pred_parallel_maxsharpe_all(returns250, 252, 5, rf=risk_free, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_week_2021_2024_250_sr$stats
saveRDS(rolling_window_results_week_2021_2024_250_sr, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/rolling_window_results_week_2021_2024_250_sr.rds")


##############################
#     Daily rebalancing      #
##############################

start.time <- Sys.time()
rolling_window_results_daily_2021_2024_50_sr <- mega_rol_pred_parallel_maxsharpe_all(returns50, 252, 1, rf=risk_free, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_daily_2021_2024_50_sr$stats
saveRDS(rolling_window_results_daily_2021_2024_50_sr, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/rolling_window_results_daily_2021_2024_50_sr.rds")


start.time <- Sys.time()
rolling_window_results_daily_2021_2024_150_sr <- mega_rol_pred_parallel_maxsharpe_all(returns150, 252, 1, rf=risk_free, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_daily_2021_2024_150_sr$stats
saveRDS(rolling_window_results_daily_2021_2024_150_sr, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/rolling_window_results_daily_2021_2024_150_sr.rds")


start.time <- Sys.time()
rolling_window_results_daily_2021_2024_250_sr <- mega_rol_pred_parallel_maxsharpe_all(returns250, 252, 1, rf=risk_free, max_factors = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
rolling_window_results_daily_2021_2024_250_sr$stats
saveRDS(rolling_window_results_daily_2021_2024_250_sr, "C:/Users/erikl_xzy542i/Documents/Master_local/Thesis/Data/rolling_window_results_daily_2021_2024_250_sr.rds")

