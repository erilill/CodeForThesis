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
  T <- nrow(returns)
  p <- ncol(returns)
  
  rebalance_dates <- seq(initial_window + 1, T, by = rebal_period)
  RT <- length(rebalance_dates)
  
  if (!is.null(rf)) {
    rf <- rf[(initial_window + 1):T]
  }
  
  m_local_list <- vector("list", RT)
  
  # Initially estimate factor count
  m_update_flags <- rep(FALSE, RT)
  days_since_last <- 0
  for (i in seq_len(RT)) {
    if (i == 1 || days_since_last >= 252) {
      m_update_flags[i] <- TRUE
      days_since_last <- 0
    } else {
      days_since_last <- days_since_last + rebal_period
    }
  }
  
  for (i in seq_len(RT)) {
    if (m_update_flags[i]) {
      current_index <- rebalance_dates[i]
      last_m <- determine_factors(returns[1:current_index, ], max_factors, silverman(returns[1:current_index,]))$optimal_m
    }
    m_local_list[[i]] <- last_m
  }
  
  # Start cluster
  cl <- parallel::makeCluster(num_cores)
  clusterExport(cl, varlist = c("returns", "rebalance_dates", "m_local_list", "rebal_period", "p", "rf", "T",
                                "initial_window", "try_invert_sample_cov", "comp_expected_returns"), envir = environment())
  
  parallel::clusterEvalQ(cl, {
    library(stats)
    library(PortfolioMoments)
    library(corpcor)
    library(POET)
    library(glasso)
    library(PerformanceAnalytics)
    library(forecast)
    library(TVMVP)
  })
  
  # Rolling in parallel
  results <- parallel::parLapply(cl, seq_len(RT), function(l) {
    current_index <- rebalance_dates[l]
    
    # Subset data for estimation
    reb_t <- rebalance_dates[l]
    est_data <- returns[1:(reb_t - 1), , drop = FALSE]
    hold_end <- min(reb_t + rebal_period - 1, T)
    m_local <- m_local_list[[l]]
    
    # Forecast mu_hat via ARIMA
    mu_hat <- comp_expected_returns(est_data, length(reb_t:hold_end))
    
    bandwidth <- silverman(est_data)
    
    # Cov from local PCA
    Sigma_tvmvp <- time_varying_cov(est_data, m_local, bandwidth)
    
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
    max_sharpe_weights <- function(Sigma, mu_hat, risk_f) {
      invS <- try_invert_sample_cov(Sigma, ridge = 1e-5)
      w_unnorm <- invS %*% (mu_hat - risk_f)
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

comp_expected_returns <- function(returns, horizon) {
  exp_ret <- numeric(ncol(returns))
  
  for (i in seq_len(ncol(returns))) {
    candidate_models <- list()
    aics <- numeric()
    
    for (order in list(c(0,0,0), c(1,0,0), c(0,0,1), c(1,0,1))) {
      model <- tryCatch(
        arima(returns[, i], order = order),
        error = function(e) NULL
      )
      candidate_models <- c(candidate_models, list(model))
      aics <- c(aics, if (!is.null(model)) AIC(model) else Inf)
    }
    
    # If all models failed, fallback to mean return
    if (all(is.infinite(aics))) {
      exp_ret[i] <- mean(returns[, i])
    } else {
      best_model <- candidate_models[[which.min(aics)]]
      fc <- predict(best_model, n.ahead = horizon)$pred
      exp_ret[i] <- mean(fc)
    }
  }
  
  return(exp_ret)
}

try_invert_sample_cov <- function(Sigma, ridge = 1e-5) {
  # Attempt a direct inversion
  inv_Sigma <- try(solve(Sigma), silent = TRUE)
  
  # Check if it failed
  if (inherits(inv_Sigma, "try-error")) {
    cat("Matrix is nearly singular; applying ridge =", ridge, "\n")
    Sigma_reg <- Sigma + ridge * diag(ncol(Sigma))
    inv_Sigma <- solve(Sigma_reg)
  }
  
  return(inv_Sigma)
}
