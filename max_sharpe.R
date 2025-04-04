################################################################################
#                           Trying out max Sharpe                              #
################################################################################
mega_rol_pred_parallel_maxsharpe_all <- function(
    returns,
    initial_window,
    rebal_period,
    max_factors,
    rf = 0,
    num_cores = parallel::detectCores() - 1
) {
  # Example: We'll use auto.arima() to forecast each asset's return
  # (You can swap for a simpler or faster method if you prefer)
  library(forecast)
  
  T <- nrow(returns)
  p <- ncol(returns)
  
  rebalance_dates <- seq(initial_window + 1, T, by = rebal_period)
  RT <- length(rebalance_dates)
  
  if (!is.null(rf)) {
    rf <- rf[(initial_window + 1):T]
  }
  
  # Initially estimate factor count
  m <- determine_factors(
    returns[1:initial_window, ], max_factors,
    silverman(returns[1:initial_window, ])
  )$optimal_m
  
  # Start cluster
  cl <- parallel::makeCluster(num_cores)
  clusterExport(cl, varlist = c("returns", "rebalance_dates", "max_factors", "m", "rebal_period", "p", "rf", 
                                "residuals", "sqrt_matrix", "compute_sigma_0", "silverman", 
                                "local_pca", "localPCA", "two_fold_convolution_kernel", 
                                "boundary_kernel", "epanechnikov_kernel", 
                                "estimate_residual_cov_poet_local", "adaptive_poet_rho", 
                                "determine_factors", "try_invert_sample_cov"), envir = environment())
  
  parallel::clusterEvalQ(cl, {
    library(PortfolioMoments)
    library(corpcor)
    library(POET)
    library(glasso)
    library(PerformanceAnalytics)
    library(forecast)
  })
  
  # Rolling in parallel
  results <- parallel::parLapply(cl, seq_len(RT), function(l) {
    current_index <- rebalance_dates[l]
    
    # Possibly re-estimate # factors every 252 days
    if ((current_index - initial_window) %% 252 == 0) {
      m_local <- determine_factors(
        returns[1:current_index, ], max_factors,
        silverman(returns[1:current_index, ])
      )$optimal_m
    } else {
      m_local <- m
    }
    
    # Subset data for estimation
    reb_t <- rebalance_dates[l]
    est_data <- returns[1:(reb_t - 1), , drop = FALSE]
    hold_end <- min(reb_t + rebal_period - 1, T)
    
    # Forecast mu_hat via ARIMA
    mu_hat <- comp_expected_returns(est_data, rebal_period)
    
    # Local PCA for Cov
    bandwidth <- silverman(est_data)
    local_res <- localPCA(est_data, bandwidth, m_local, epanechnikov_kernel)
    
    # Cov from local PCA + POET
    Sigma_tvmvp <- estimate_residual_cov_poet_local(
      localPCA_results = local_res,
      returns = est_data,
      M0 = 10,
      rho_grid = seq(0.005, 2, length.out = 30),
      floor_value = 1e-12,
      epsilon2 = 1e-6
    )$total_cov
    
    # 1) Sample Cov
    Sigma_sample <- cov(est_data)
    
    # 2) Shrink Cov
    Sigma_shrink <- corpcor::cov.shrink(est_data)
    
    # 3) EWMA Cov
    Sigma_ewma <- PortfolioMoments::cov_ewma(est_data, lambda = 0.94)
    
    # 4) POET Cov
    poet_res <- POET(t(est_data), m_local)
    Sigma_POET <- poet_res$SigmaY
    
    # 5) Glasso Cov
    S <- cov(est_data)
    glasso_out <- glasso::glasso(S, rho = 0.01)
    Sigma_glasso <- glasso_out$w
    
    
    # Helper to do max sharpe:
    # w_maxsharpe ~ inv(Sigma) * (mu_hat - rf)
    # Then normalize
    max_sharpe_weights <- function(Sigma, mu_hat, rf) {
      invS <- solve(Sigma)
      w_unnorm <- invS %*% (mu_hat - rf)
      as.numeric(w_unnorm / sum(w_unnorm))
    }
    
    w_sample_max  <- max_sharpe_weights(Sigma_sample, mu_hat, rf[reb_t - initial_window])
    w_shrink_max  <- max_sharpe_weights(Sigma_shrink, mu_hat, rf[reb_t - initial_window])
    w_ewma_max    <- max_sharpe_weights(Sigma_ewma, mu_hat, rf[reb_t - initial_window])
    w_poet_max    <- max_sharpe_weights(Sigma_POET, mu_hat, rf[reb_t - initial_window])
    w_glasso_max  <- max_sharpe_weights(Sigma_glasso, mu_hat, rf[reb_t - initial_window])
    w_tvmvp_max     <- max_sharpe_weights(Sigma_tvmvp, mu_hat, rf[reb_t - initial_window])
    
    # Holding window
    ret_window <- returns[reb_t:hold_end, , drop = FALSE]
    
    # Return daily returns of each method's max sharpe
    list(
      daily_ret_sample_max  = ret_window %*% w_sample_max,
      daily_ret_shrink_max  = ret_window %*% w_shrink_max,
      daily_ret_ewma_max    = ret_window %*% w_ewma_max,
      daily_ret_poet_max    = ret_window %*% w_poet_max,
      daily_ret_glasso_max  = ret_window %*% w_glasso_max,
      daily_ret_tvmvp       = ret_window %*% w_tvmvp_max
    )
  })
  
  parallel::stopCluster(cl)
  
  # Unlist daily returns
  daily_ret_sample_max <- unlist(lapply(results, `[[`, "daily_ret_sample_max"))
  daily_ret_shrink_max <- unlist(lapply(results, `[[`, "daily_ret_shrink_max"))
  daily_ret_ewma_max   <- unlist(lapply(results, `[[`, "daily_ret_ewma_max"))
  daily_ret_poet_max   <- unlist(lapply(results, `[[`, "daily_ret_poet_max"))
  daily_ret_glasso_max <- unlist(lapply(results, `[[`, "daily_ret_glasso_max"))
  daily_ret_tvmvp      <- unlist(lapply(results, `[[`, "daily_ret_tvmvp"))
  
  # Compute excess returns if rf is a vector:
  er_sample_max <- daily_ret_sample_max - rf
  er_shrink_max <- daily_ret_shrink_max - rf
  er_ewma_max   <- daily_ret_ewma_max   - rf
  er_poet_max   <- daily_ret_poet_max   - rf
  er_glasso_max <- daily_ret_glasso_max - rf
  er_tvmvp      <- daily_ret_tvmvp    - rf
  
  # Summarize stats
  compute_metrics <- function(x) {
    c(
      CER    = sum(x),
      Mean   = mean(x),
      SD     = sd(x),
      Sharpe = mean(x)/sd(x)
    )
  }
  
  sample_stats  <- compute_metrics(er_sample_max)
  shrink_stats  <- compute_metrics(er_shrink_max)
  ewma_stats    <- compute_metrics(er_ewma_max)
  poet_stats    <- compute_metrics(er_poet_max)
  glasso_stats  <- compute_metrics(er_glasso_max)
  tvmvp_stats     <- compute_metrics(er_tvmvp)
  
  stats_df <- data.frame(
    Method = c("SampleCov-MaxSharpe","ShrinkCov-MaxSharpe","EWMA-MaxSharpe",
               "POET-MaxSharpe","Glasso-MaxSharpe","TVMVP-MaxSharpe"),
    CER    = c(sample_stats["CER"], shrink_stats["CER"], ewma_stats["CER"],
               poet_stats["CER"], glasso_stats["CER"], tvmvp_stats["CER"]),
    Mean   = c(sample_stats["Mean"], shrink_stats["Mean"], ewma_stats["Mean"],
               poet_stats["Mean"], glasso_stats["Mean"], tvmvp_stats["Mean"]),
    SD     = c(sample_stats["SD"], shrink_stats["SD"], ewma_stats["SD"],
               poet_stats["SD"], glasso_stats["SD"], tvmvp_stats["SD"]),
    Sharpe = c(sample_stats["Sharpe"], shrink_stats["Sharpe"], ewma_stats["Sharpe"],
               poet_stats["Sharpe"], glasso_stats["Sharpe"], tvmvp_stats["Sharpe"])
  )
  
  # Return daily returns + summary
  list(
    daily_returns = list(
      sample_cov = daily_ret_sample_max,
      shrink_cov = daily_ret_shrink_max,
      ewma       = daily_ret_ewma_max,
      poet       = daily_ret_poet_max,
      glasso     = daily_ret_glasso_max,
      tvmvp      = daily_ret_tvmvp
    ),
    stats = stats_df
  )
}
