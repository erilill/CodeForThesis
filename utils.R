# Necessary functions from package
silverman <- function(returns){
  ip <- ncol(returns)
  iT <- nrow(returns)
  bandwidth <- (2.35/sqrt(12)) * iT^(-0.2) * ip^(-0.1)
  return(bandwidth)
}
sqrt_matrix <- function(Amat) {
  eig <- eigen(Amat)
  eig_values_sqrt <- diag(sqrt(abs(eig$values)))  # Ensure non-negative eigenvalues
  return(eig$vectors %*% eig_values_sqrt %*% t(eig$vectors))
}

compute_sigma_0 <- function(res_set, iT, ip) {
  sigma_0 <- crossprod(res_set) / iT
  
  for (i in 1:(ip - 1)) {
    for (j in (i + 1):ip) {
      sigma_0[i, j] <- sigma_0[i, j] * ((1 - 0.01)^(j - i))
      sigma_0[j, i] <- sigma_0[i, j]
    }
  }
  
  return(sigma_0)
}
epanechnikov_kernel <- function(u) {
  ifelse(abs(u) <= 1, 0.75 * (1 - u^2), 0)
}
boundary_kernel <- function(t, r, iT, h, kernel_func) {
  scaled_diff <- (t - r) / (iT * h)
  k_val <- kernel_func(scaled_diff) / h
  
  # Determine the region of r
  Th_floor <- floor(iT * h)
  
  if (r < Th_floor) {
    # Lower boundary case
    integral_val <- integrate(kernel_func, lower = -r / (iT * h), upper = 1)$value
    return(k_val / integral_val)
  } else if (r > (iT - Th_floor)) {
    # Upper boundary case
    integral_val <- integrate(kernel_func, lower = -1, upper = (1 - r / iT) / h)$value
    return(k_val / integral_val)
  } else {
    # Middle region
    return(k_val)
  }
}

two_fold_convolution_kernel <- function(u, kernel_func) {
  result <- ifelse(
    abs(u) <= 2,
    sapply(u, function(u_val) {
      integrand <- function(v) kernel_func(v) * kernel_func(u_val - v)
      integrate(integrand, lower = -1, upper = 1)$value
    }),
    0
  )
  return(result)
}
estimate_residual_cov_poet_local <- function(localPCA_results, 
                                             returns,
                                             M0 = 10, 
                                             rho_grid = seq(0.005, 2, length.out = 30),
                                             floor_value = 1e-12,
                                             epsilon2 = 1e-6) {
  
  # This function:
  #   1. Form local residuals e_t = R - F * Lambda_t
  #   2. Call adaptive_poet_rho() on those residuals to pick the single best rho_t
  #   3. Compute raw residual covariance, shrink once using rho_t
  #   4. Combine with factor part to get Sigma_R(t)
  #
  # The function returns a list with the final local residual and total covariances.
  
  
  
  # 1. Extract local loadings, factors, and row indices
  Lambda_t <- localPCA_results$loadings[[nrow(returns)]]  # p x m
  F_t <- localPCA_results$f_hat   # T x m
  w_t <- localPCA_results$weights[[nrow(returns)]]
  
  # 2. residuals
  U_local <- residuals(F_t, localPCA_results$loadings, returns)  # T x p
  
  # 3. Pick best rho for these local residuals using Chen–Leng–style grouping
  #    This returns (best_rho = ..., min_Fnorm = ...)
  rho_result <- adaptive_poet_rho(U_local,
                                  M0 = M0,
                                  rho_grid = rho_grid,
                                  epsilon2 = epsilon2)
  best_rho_t <- rho_result$best_rho
  
  # 4. Compute the naive residual covariance, then shrink once
  S_u_raw <- (1 / nrow(U_local)) * crossprod(U_local)  # p x p
  # threshold based on best_rho_t
  threshold <- best_rho_t * mean(abs(S_u_raw[upper.tri(S_u_raw)]))
  
  # soft-threshold function
  soft_threshold <- function(x, thr) {
    sign(x) * pmax(abs(x) - thr, 0)
  }
  # apply to off-diagonal entries
  S_u_shrunk <- apply(S_u_raw, c(1, 2), soft_threshold, thr = threshold)
  # keep diagonal as-is
  diag(S_u_shrunk) <- diag(S_u_raw)
  
  # 5. Final local covariance = factor part + shrunk residual
  Sigma_R_t <- Lambda_t%*%(crossprod(F_t)/nrow(returns))%*%t(Lambda_t) + S_u_shrunk  # p x p
  
  #Please check /Erik
  ############################################################################
  # Final PSD repair
  e_decomp <- eigen(Sigma_R_t, symmetric = TRUE)
  eigvals  <- e_decomp$values
  eigvecs  <- e_decomp$vectors
  eigvals_floored <- ifelse(eigvals < floor_value, floor_value, eigvals)
  Sigma_R_t <- eigvecs %*% diag(eigvals_floored) %*% t(eigvecs) # reconstruct sigma
  Sigma_R_t <- 0.5 * (Sigma_R_t + t(Sigma_R_t)) #Symmetrize
  ############################################################################
  
  
  # store results
  output_list <- list(
    best_rho        = best_rho_t,
    residual_cov    = S_u_shrunk,  # Sigma e(T)
    total_cov       = Sigma_R_t,   # Sigma R(T)
    loadings        = Lambda_t,
    naive_resid_cov = S_u_raw 
  )
  
  
  return(output_list)
}
adaptive_poet_rho <- function(R, M0 = 10,
                              rho_grid = seq(0.001, 2, length.out = 20),
                              epsilon2 = 1e-6) {
  # R: data matrix, dimension T x p
  # M0: number of observations to leave out between the two sub-samples
  # rho_grid: grid of possible rho values
  
  iT <- nrow(R)
  ip <- ncol(R)
  
  # Half of the sample size (floored)
  halfT <- floor(iT / 2)
  
  # Define T1 and T2 for the sub-samples
  T1 <- floor(halfT * (1 - 1 / log(iT)))
  T2 <- halfT - T1  # ensures T1 + T2 = floor(T/2)
  
  # Number of groups as per Chen et al. (2019), p. 61:
  # "we divide the full sample into floor(T/(2*M0)) groups"
  num_groups <- floor(iT / (2 * M0))
  
  if (num_groups < 1) {
    stop("Not enough data for adaptive rho selection. Increase T or reduce M0.")
  }
  
  # A small helper for soft-thresholding
  soft_threshold <- function(x, thr) {
    sign(x) * pmax(abs(x) - thr, 0)
  }
  
  # Function to compute the SUM of Frobenius-norm differences
  # across all groups for a given rho
  frob_sum_for_rho <- function(rho) {
    total_error <- 0
    lambda_min_vals <- numeric(num_groups)
    
    for (m in seq_len(num_groups)) {
      # The m-th group includes observations from:
      #    start_idx = (m - 1)*M0 + 1
      #    end_idx   = (m - 1)*M0 + (halfT + M0)
      #
      # each group is of length halfT + M0, with M0 overlap/step.
      
      start_idx <- (m - 1) * M0 + 1
      end_idx   <- (m - 1) * M0 + (halfT + M0)
      if (end_idx > iT) break  # guard for edge cases
      
      # Sub-sample 1: first T1 observations of the group
      sub1_start <- start_idx
      sub1_end   <- start_idx + T1 - 1
      
      # Skip M0 observations after sub1
      # sub-sample 2: last T2 observations
      sub2_start <- start_idx + T1 + M0
      sub2_end   <- sub2_start + T2 - 1
      
      if (sub2_end > end_idx) break  # guard for edge cases
      
      # Extract the actual data for sub-samples
      data_sub1 <- R[sub1_start:sub1_end, , drop = FALSE]
      data_sub2 <- R[sub2_start:sub2_end, , drop = FALSE]
      
      # Covariance from the first sub-sample: will be shrunk
      S1 <- cov(data_sub1)
      # Covariance from the second sub-sample: "naive" benchmark
      S2 <- cov(data_sub2)
      
      # Apply soft-thresholding to S1 based on rho
      threshold <- rho * mean(abs(S1[upper.tri(S1)]))
      S1_shrunk <- apply(S1, c(1, 2), soft_threshold, thr = threshold)
      
      eigvals <- eigen(S1_shrunk, symmetric = TRUE, only.values = TRUE)$values
      lambda_min_vals[m] <- min(eigvals)
      
      # Accumulate Frobenius norm difference
      total_error <- total_error + sum((S1_shrunk - S2)^2)
    }
    
    return(list(total_error = total_error, lambda_min_vals = lambda_min_vals))
  }
  
  # We now scan across rho_grid, compute the total Frobenius difference,
  # and pick the single rho that MINIMIZES the sum across all groups.
  
  best_rho <- NA
  min_val  <- Inf
  lambda_min_all <- numeric(length(rho_grid))
  for (i in seq_along(rho_grid)){
    rho_result <- frob_sum_for_rho(rho_grid[i])
    lambda_min_all[i] <- min(rho_result$lambda_min_vals, na.rm=iT)
  }
  
  # Compute rho_1
  valid_rho_indices <- which(!is.na(lambda_min_all) & lambda_min_all > 0)
  rho_1 <- if (length(valid_rho_indices) > 0) {
    epsilon2 + min(rho_grid[valid_rho_indices])
  } else {
    epsilon2  # Default to epsilon2 if no valid rho is found
  }
  
  # Compute rho
  for (rho in rho_grid[rho_grid >= rho_1]) {
    rho_result <- frob_sum_for_rho(rho)
    val <- rho_result$total_error
    
    if (val < min_val) {
      min_val  <- val
      best_rho <- rho
    }
  }
  return(list(best_rho = best_rho, rho_1 = rho_1, min_Fnorm = min_val))
}


determine_factors <- function(returns, max_m, bandwidth) {
  iT <- nrow(returns)
  ip <- ncol(returns)
  
  # Initialize storage
  V <- numeric(max_m)
  penalty <- numeric(max_m)
  IC_values <- numeric(max_m)
  
  # Loop over possible number of factors (R)
  for (mi in 1:max_m) {
    residuals <- matrix(NA, nrow = iT, ncol = ip)
    prev_F = NULL
    for (r in 1:iT){
      # Step 1: Perform PCA with R factors
      pca_result <- try(local_pca(returns, r = r, bandwidth = bandwidth, 
                                  m = mi, kernel_func = epanechnikov_kernel, 
                                  prev_F))
      if("try-error" %in% class(pca_result))
      {
        next
      }
      
      
      X_r <- matrix(0, nrow = iT, ncol = ip)
      X_r <- sweep(returns, 1, sqrt(pca_result$w_r), `*`)
      scaled_loadings <- sqrt(ip) * sweep(pca_result$loadings, 2, sqrt(colSums(pca_result$loadings^2)), "/")
      Lambda_breve_R <- t((1/(iT*ip))*t(X_r)%*%X_r%*%scaled_loadings)
      F_breve_R <- solve((Lambda_breve_R)%*%t(Lambda_breve_R))%*%(Lambda_breve_R)%*%returns[r,]
      
      # Step 2: Compute SSR (Sum of Squared Residuals)
      residuals[r,] <- returns[r,] - t(F_breve_R) %*% (Lambda_breve_R)
      
      prev_F <- pca_result$F_hat_r
    }
    V[mi] <- sum(residuals^2) / (ip * iT)
    penalty[mi] <- mi * ((ip+iT*bandwidth)/(ip*iT*bandwidth))*log((ip*iT*bandwidth)/(ip+iT*bandwidth))
    IC_values[mi] <- log(V[mi]) + penalty[mi]
  }
  # Step 4: Determine optimal number of factors
  optimal_m <- which.min(IC_values)
  #message(sprintf("Optimal number of factors is %s.", optimal_R))
  return(list(optimal_m = optimal_m, IC_values = IC_values))
}

local_pca <- function(returns, r, bandwidth, m, kernel_func, prev_F = NULL) {
  iT <- nrow(returns)
  ip <- ncol(returns)
  
  # Compute Kernel Weights
  k_h <- sapply(1:iT, function(t) boundary_kernel(r, t, iT, bandwidth, kernel_func))
  X_r <- sweep(returns, 1, sqrt(k_h), `*`)  # Weighted returns
  
  # Compute Eigen Decomposition
  eigen_txr_xr <- eigen((X_r) %*% t(X_r))
  idx <- order(eigen_txr_xr$values, decreasing = TRUE)
  eigvals <- eigen_txr_xr$values[idx]
  eigvecs <- eigen_txr_xr$vectors[, idx]
  
  # Enforce Orthonormality of Factors (F_r)
  F_hat_r <- sqrt(iT) * eigvecs[, 1:m, drop = FALSE]  # (T x m)
  
  # Align eigenvector directions if previous factors exist
  if (!is.null(prev_F)) {
    for (j in 1:m) {
      if (cor(prev_F[, j], F_hat_r[, j]) < 0) {
        F_hat_r[, j] <- -F_hat_r[, j]  # Flip sign for consistency
      }
    }
  }
  
  # Compute Loadings: Lambda_r
  t_lambda_hat_r <- t(F_hat_r) %*% (X_r) / iT  # (m x p)
  loadings <- t(t_lambda_hat_r)
  
  # Second pass to compute F_r_hat
  part1 <- crossprod(loadings)   # (m x m)
  part2 <- crossprod(loadings, returns[r, ]) # (m)
  F_r_hat <- solve(part1, part2)        # (m)
  
  return(list(
    factors = F_hat_r,  # (T x m)
    f_hat = t(F_r_hat), # (1 x m)
    loadings = loadings,  # (p x m)
    w_r = as.matrix(k_h)
  ))
}
localPCA <- function(returns,
                     bandwidth,
                     m,
                     kernel_func = epanechnikov_kernel) {
  ip <- ncol(returns)
  iT <- nrow(returns)
  
  # Initialize storage
  factors <- vector("list", iT)
  loadings <- vector("list", iT)
  weights_list <- vector("list", iT)
  f_hat <- matrix(NA, nrow=iT, ncol=m)
  
  prev_F <- NULL
  
  # For each time t, do local PCA
  for (t_i in 1:iT) {
    local_result <- local_pca(returns, t_i, bandwidth, m, kernel_func, prev_F)
    factors[[t_i]] <- local_result$factors
    loadings[[t_i]] <- local_result$loadings
    weights_list[[t_i]] <- local_result$w_r
    f_hat[t_i,] <- local_result$f_hat
    
    prev_F <- local_result$factors
  }
  
  return(list(
    factors = factors,    # T x m
    loadings = loadings,  # list of length T, each p x m
    m = m,
    weights = weights_list,
    f_hat=f_hat
  ))
}


residuals <- function(factors, loadings_list, returns) {
  iT <- nrow(returns)
  ip <- ncol(returns)
  
  residuals <- matrix(NA, nrow = iT, ncol = ip)
  
  for (t in 1:iT) {
    factors_t <- factors[t, , drop = FALSE]
    loadings_t <- (loadings_list[[t]])
    
    modeled_returns_t <- factors_t %*% t(loadings_t)
    residuals[t, ] <- returns[t, ] - modeled_returns_t
  }
  return(residuals)
}

#' Function to compute expected returns using a simple model selection approach
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
