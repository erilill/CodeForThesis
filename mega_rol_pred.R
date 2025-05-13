
# Constructing function for rolling window comparison, not for package
# Compare TVMVP with MVP constructed with other methods, i.e. sample cov, poet, EMWA, shrinkage, etc

library(parallel)
library(PortfolioMoments)
library(corpcor)
library(POET)
library(glasso)
library(PerformanceAnalytics)

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

mega_rol_pred_parallel <- function(returns,
                                   initial_window,   
                                   rebal_period,     
                                   max_factors,
                                   rf = 0,
                                   num_cores = detectCores() - 1) {
  # Dimensions
  T <- nrow(returns)
  p <- ncol(returns)
  
  # Define rebalancing dates before starting the cluster
  rebalance_dates <- seq(initial_window + 1, T, by = rebal_period)
  RT <- length(rebalance_dates)
  
  # Process risk-free rate vector
  if (!is.null(rf)) {
    rf <- rf[(initial_window + 1):T]
  }
  
  m_local_list <- vector("list", RT)
  last_m <- determine_factors(returns[1:initial_window, ], max_factors, silverman(returns[1:initial_window,]))$optimal_m
  
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
  
  
  
  # Start parallel cluster
  cl <- makeCluster(num_cores)
  clusterExport(cl, varlist = c("returns", "m_local_list", "rebalance_dates", "rebal_period", "p", "T", "rf", 
                                "initial_window", "try_invert_sample_cov"), envir = environment())
  clusterEvalQ(cl, {
    library(PortfolioMoments)
    library(corpcor)
    library(POET)
    library(glasso)
    library(PerformanceAnalytics)
    library(TVMVP)
  })
  
  results <- parLapply(cl, seq_len(RT),
                       function(l, rebalance_dates, rebal_period, p, rf, returns, initial_window) {
                         
                         m_local <- m_local_list[[l]]
                         
                         reb_t <- rebalance_dates[l]
                         est_data <- returns[1:(reb_t - 1), , drop = FALSE]
                         
                         # For local PCA, re-estimate bandwidth using estimation data
                         bandwidth <- silverman(est_data)
                         
                         # Compute covariance using the local PCA results
                         Sigma_hat <- time_varying_cov(est_data, m_local, bandwidth)
                         
                         # Compute GMVP weights (and other methods)
                         inv_cov <- chol2inv(chol(Sigma_hat))
                         ones <- rep(1, p)
                         w_gmv_unnorm <- inv_cov %*% ones
                         w_gmv <- as.numeric(w_gmv_unnorm / sum(w_gmv_unnorm))
                         
                         # Sample Covariance
                         Sigma_sample <- cov(est_data)
                         inv_sample <- try_invert_sample_cov(Sigma_sample, ridge = 1e-5) # fails when p>T
                         w_sample <- as.numeric(inv_sample %*% rep(1, p))
                         w_sample <- w_sample / sum(w_sample)
                         
                         # Shrinkage Covariance
                         Sigma_shrink <- corpcor::cov.shrink(est_data)
                         inv_shrink <- solve(Sigma_shrink)
                         w_shrink <- as.numeric(inv_shrink %*% rep(1, p))
                         w_shrink <- w_shrink / sum(w_shrink)
                         
                         # EWMA Covariance
                         lambda <- 0.94
                         Sigma_emwa <- PortfolioMoments::cov_ewma(est_data, lambda = lambda)
                         inv_emwa <- solve(Sigma_emwa)
                         w_emwa <- as.numeric(inv_emwa %*% rep(1, p))
                         w_emwa <- w_emwa / sum(w_emwa)
                         
                         # POET Covariance
                         poet_res <- POET(t(est_data), m_local)
                         Sigma_POET <- poet_res$SigmaY
                         inv_POET <- solve(Sigma_POET)
                         w_POET <- as.numeric(inv_POET %*% rep(1, p))
                         w_POET <- w_POET / sum(w_POET)
                         
                         # Glasso Covariance
                         S <- cov(est_data)
                         glasso_res <- glasso::glasso(S, rho = 0.01)
                         Sigma_glasso <- glasso_res$w
                         inv_glasso <- solve(Sigma_glasso)
                         w_glasso <- as.numeric(inv_glasso %*% rep(1, p))
                         w_glasso <- w_glasso / sum(w_glasso)
                         
                         
                         # Define the holding window for returns
                         hold_end <- min(reb_t + rebal_period - 1, T)
                         ret_window <- returns[reb_t:hold_end, , drop = FALSE]
                         
                         list(
                           daily_ret_equal = ret_window %*% rep(1/p, p),
                           daily_ret_sample = ret_window %*% w_sample,
                           daily_ret_shrink = ret_window %*% w_shrink,
                           daily_ret_emwa = ret_window %*% w_emwa,
                           daily_ret_POET = ret_window %*% w_POET,
                           daily_ret_glasso = ret_window %*% w_glasso,
                           daily_ret_tvmvp = ret_window %*% w_gmv
                         )
                       },
                       rebalance_dates = rebalance_dates, rebal_period = rebal_period, p = p, rf = rf,
                       returns = returns, initial_window = initial_window
  )
  
  stopCluster(cl)  # Stop the parallel cluster
  
  # Extract daily returns for each method
  daily_ret_equal   <- unlist(lapply(results, `[[`, "daily_ret_equal"))
  daily_ret_sample  <- unlist(lapply(results, `[[`, "daily_ret_sample"))
  daily_ret_shrink  <- unlist(lapply(results, `[[`, "daily_ret_shrink"))
  daily_ret_emwa    <- unlist(lapply(results, `[[`, "daily_ret_emwa"))
  daily_ret_POET    <- unlist(lapply(results, `[[`, "daily_ret_POET"))
  daily_ret_glasso  <- unlist(lapply(results, `[[`, "daily_ret_glasso"))
  daily_ret_tvmvp   <- unlist(lapply(results, `[[`, "daily_ret_tvmvp"))
  
  # Compute excess returns
  er_equal   <- daily_ret_equal   - rf
  er_sample  <- daily_ret_sample  - rf
  er_shrink  <- daily_ret_shrink  - rf
  er_emwa    <- daily_ret_emwa    - rf
  er_POET    <- daily_ret_POET    - rf
  er_glasso  <- daily_ret_glasso  - rf
  er_tvmvp   <- daily_ret_tvmvp   - rf
  
  compute_metrics <- function(er) {
    # Convert excess returns from log returns to simple returns
    simple_returns <- exp(er) - 1  # Transform log returns to simple returns
    cumulative_simple_returns <- cumprod(1+simple_returns)
    running_max <- cummax(cumulative_simple_returns)
    drawdowns_numeric <- 1-cumulative_simple_returns/running_max
    max_drawdown <- max(drawdowns_numeric)
    
    
    CER <- sum(er) # still log
    mean <- mean(er) # still log
    sd <- sqrt(var(er))
    sharpe <- mean / sd
    list(
      CER = CER, 
      mean_excess = mean,
      sd = sd, # Risk
      sharpe = sharpe,
      MDD = max_drawdown,
      cum_er = cumulative_simple_returns,
      drawdowns = drawdowns_numeric
    )
  }
  
  
  stats_equal  <- compute_metrics(er_equal)
  stats_sample <- compute_metrics(er_sample)
  stats_shrink <- compute_metrics(er_shrink)
  stats_emwa   <- compute_metrics(er_emwa)
  stats_POET   <- compute_metrics(er_POET)
  stats_glasso <- compute_metrics(er_glasso)
  stats_tvmvp  <- compute_metrics(er_tvmvp)
  
  methods_stats <- data.frame(
    method = c("1/N", "SampleCov", "ShrinkCov", "EWMA", "POET", "Glasso", "TV-MVP"),
    cumulative_excess = c(stats_equal$CER,
                          stats_sample$CER,
                          stats_shrink$CER,
                          stats_emwa$CER,
                          stats_POET$CER,
                          stats_glasso$CER,
                          stats_tvmvp$CER),
    mean_excess = c(stats_equal$mean_excess,
                    stats_sample$mean_excess,
                    stats_shrink$mean_excess,
                    stats_emwa$mean_excess,
                    stats_POET$mean_excess,
                    stats_glasso$mean_excess,
                    stats_tvmvp$mean_excess),
    sd = c(stats_equal$sd,
           stats_sample$sd,
           stats_shrink$sd,
           stats_emwa$sd,
           stats_POET$sd,
           stats_glasso$sd,
           stats_tvmvp$sd),
    sharpe = c(stats_equal$sharpe,
               stats_sample$sharpe,
               stats_shrink$sharpe,
               stats_emwa$sharpe,
               stats_POET$sharpe,
               stats_glasso$sharpe,
               stats_tvmvp$sharpe),
    MDD = c(stats_equal$MDD,
            stats_sample$MDD,
            stats_shrink$MDD,
            stats_emwa$MDD,
            stats_POET$MDD,
            stats_glasso$MDD,
            stats_tvmvp$MDD)
  )
  
  list(
    daily_returns = list(equal = daily_ret_equal,
                         sample_cov = daily_ret_sample,
                         shrink_cov = daily_ret_shrink,
                         EWMA = daily_ret_emwa,
                         POET = daily_ret_POET,
                         glasso = daily_ret_glasso,
                         tvmvp = daily_ret_tvmvp),
    cumulative_simple_returns = list(equal = stats_equal$cum_er,
                                     sample_cov = stats_sample$cum_er,
                                     shrink_cov = stats_shrink$cum_er,
                                     EWMA = stats_emwa$cum_er,
                                     POET = stats_POET$cum_er,
                                     glasso = stats_equal$cum_er,
                                     tvmvp = stats_tvmvp$cum_er),
    drawdowns = list(equal = stats_equal$cum_er,
                     sample_cov = stats_sample$drawdowns,
                     shrink_cov = stats_shrink$drawdowns,
                     EWMA = stats_emwa$drawdowns,
                     POET = stats_POET$drawdowns,
                     glasso = stats_glasso$drawdowns,
                     tvmvp = stats_tvmvp$drawdowns),
    num_factors = m_local_list,
    stats = methods_stats
  )
}

