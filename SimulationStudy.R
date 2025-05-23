####################
# Simulation study #
####################

library(TVMVP)
library(MASS)


# Will mostly be copying Su and Wang DGP1-6, then two simulated based on real data if I can find data

###############
#### DGP's ####
###############

# Factors
generate_factors <- function(T) {
  F1 <- numeric(T)
  F2 <- numeric(T)
  F1[1] <- rnorm(1, mean = 0, sd = sqrt(1 / (1 - 0.6^2)))
  F2[1] <- rnorm(1, mean = 0, sd = sqrt(1 / (1 - 0.3^2)))
  
  for (t in 2:T) {
    F1[t] <- 0.6 * F1[t - 1] + rnorm(1, mean = 0, sd = sqrt(1 - 0.6^2))
    F2[t] <- 0.3 * F2[t - 1] + rnorm(1, mean = 0, sd = sqrt(1 - 0.3^2))
  }
  
  return(cbind(F1, F2))
}

#DGP 1 IID
generate_DGP1 <- function(p, T, F){
  X <- matrix(NA, nrow=T, ncol=p)
  for (t in 1:T){
    Lambda <- MASS::mvrnorm(p, mu=rep(0,2), Sigma = diag(1,2))
    e <- rnorm(p, mean=0, sd=1)
    X[t,] <- t(Lambda %*% F[t,] + e)
  }
  return(X)
}

# DGP 2 heteroskedasticity
generate_DGP2 <- function(p, T, F){
  X <- matrix(NA, nrow=T, ncol=p)
  for (t in 1:T){
    Lambda <- MASS::mvrnorm(p, mu=rep(0,2), Sigma = diag(1,2))
    sigma_i <- runif(p, 0.5, 1.5)
    e <- rnorm(p, mean=0, sd=1)*sigma_i
    X[t,] <- t(Lambda %*% F[t,] + e)
  }
  return(X)
}

# DGP 3 Cross-sectional dependence
generate_DGP3 <- function(p, T, F){
  X <- matrix(NA, nrow=T, ncol=p)
  for (t in 1:T){
    Lambda <- MASS::mvrnorm(p, mu=rep(0,2), Sigma = diag(1,2))
    Sigma_e <- outer(1:p, 1:p, function(i, j) 0.5^abs(i-j))
    e <- MASS::mvrnorm(1, mu=rep(0,p), Sigma=Sigma_e)
    X[t,] <- t(Lambda %*% F[t,] + e)
  }
  return(X)
}

# DGP 4 Single structural brake
generate_DGP4 <- function(p, T, F, b = 2) {
  X <- matrix(NA, nrow=T, ncol=p)
  Lambda1 <- matrix(rnorm(p * 2, mean=1, sd=1), ncol = 2)
  Lambda2 <- Lambda1 + b
  
  for (t in 1:T) {
    if (t <= T/2) {
      Lambda <- Lambda1
    } else {
      Lambda <- Lambda2
    }
    sigma_i <- runif(p, 0.5, 1.5)
    e <- rnorm(p) * sigma_i
    
    X[t, ] <- t(Lambda %*% F[t, ] + e)
  }
  return(X)
}



# DGP 5 Multiple Structural Breaks
generate_DGP5 <- function(p, T, F, b = 2) {
  Lambda_base <- cbind(
    rnorm(p, mean = 1, sd = 1),  # Column 1
    rnorm(p, mean = 0, sd = 1)   # Column 2
  )
  Lambda_plus  <- Lambda_base; Lambda_plus[,1]  <- Lambda_plus[,1]  + 0.5 * b
  Lambda_minus <- Lambda_base; Lambda_minus[,1] <- Lambda_minus[,1] - 0.5 * b
  
  X <- matrix(NA, nrow = T, ncol = p)
  for (t in seq_len(T)) {
    # By default: no shift
    Lambda_t <- Lambda_base
    if (t > 0.2 * T && t <= 0.4 * T) {
      Lambda_t <- Lambda_minus
    }
    else if (t > 0.6 * T && t <= 0.8 * T) {
      Lambda_t <- Lambda_plus
    }
    e <- rnorm(p, mean = 0, sd = 1)
    X[t, ] <- t(Lambda_t %*% F[t, ] + e)
  }
  return(X)
}


# DGP 6 Smooth Structural Changes I
generate_DGP6 <- function(p, T, F, b = 2) {
  X <- matrix(NA, nrow = T, ncol = p)
  Lambda1 <- rnorm(p, mean = 0, sd = 1)
  G <- function(x, a, b) {
    1 / (1 + exp(-a * (x - b)))
  }
  for (t in seq_len(T)) {
    lam2 <- numeric(p)
    for (i in seq_len(p)) {
      lam2[i] <- b * G(10 * t / T, 2, 5 * i / p + 2)
    }
    Lambda_t <- cbind(Lambda1, lam2)
    e <- rnorm(p, mean = 0, sd = 1)
    X[t, ] <- t(Lambda_t %*% F[t, ] + e)
  }
  
  return(X)
}


################################################################################
# run simulation for hypothesis test:
library(parallel)

# Set up
set.seed(1337)
T <- 200
p <- 100
R <- 500
m <- 2
B <- 200

# Generate factors
F_t <- generate_factors(T)

##############
#    DGP1    #
##############

# Set up a cluster using all available cores
cl <- makeCluster(detectCores()-1)

# Export required functions and objects to the cluster's environment.
# Make sure to include all functions (generate_DGP1, hyptest1, etc.) 
# and objects (F_t, T, p, m, B) that will be used inside the worker function.
clusterExport(cl, varlist = c("generate_DGP1", "residuals","sqrt_matrix", "hyptest1", "compute_sigma_0", "silverman", "local_pca", "localPCA", "two_fold_convolution_kernel", "boundary_kernel", "compute_B_pT", "compute_M_hat", "compute_V_pT", "epanechnikov_kernel", "F_t", "T", "p", "m", "B", "R"))
clusterEvalQ(cl, {
  library(TVMVP)
  library(MASS)
})

start.time <- Sys.time()
# Run the simulation in parallel using parLapply.
# Each worker generates synthetic data and runs the hypothesis test.
results_list_dgp1 <- parLapply(cl, 1:R, function(r) {
  # Generate synthetic data for replication r
  X_sim <- generate_DGP1(p, T, F_t)
  
  # Run the hypothesis test on the synthetic data
  test_result <- hyptest1(X_sim, m, B)
  
  # Return the relevant results as a list
  list(
    J_NT = test_result$J_NT,
    p_value = test_result$p_value,
    reject_H0 = test_result$p_value < 0.05
  )
})
# Shut down the cluster
stopCluster(cl)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Combine the list of results into a data frame
test_results_dgp1 <- do.call(rbind, lapply(results_list_dgp1, as.data.frame))

# Compute the rejection rate
rejection_rate <- mean(test_results_dgp1$reject_H0)

# Print summary statistics and a histogram of p-values
print(summary(test_results_dgp1$p_value))
hist(test_results_dgp1$p_value, breaks = 20, col = "lightblue", 
     main = "Histogram of p-values", xlab = "p-value", ylab = "Frequency", border = "black")

# Optionally, print the rejection rate
print(sprintf("Rejection rate: %.4f", rejection_rate))

##############
#    DGP2    #
##############

# Set up a cluster using all available cores
cl <- makeCluster(detectCores()-1)

# Export required functions and objects to the cluster's environment.
# Make sure to include all functions (generate_DGP1, hyptest1, etc.) 
# and objects (F_t, T, p, m, B) that will be used inside the worker function.
clusterExport(cl, varlist = c("generate_DGP2", "residuals","sqrt_matrix", "hyptest1", "compute_sigma_0", "silverman", "local_pca", "localPCA", "two_fold_convolution_kernel", "boundary_kernel", "compute_B_pT", "compute_M_hat", "compute_V_pT", "epanechnikov_kernel", "F_t", "T", "p", "m", "B", "R"))
clusterEvalQ(cl, {
  library(TVMVP)
  library(MASS)
})

start.time <- Sys.time()
# Run the simulation in parallel using parLapply.
# Each worker generates synthetic data and runs the hypothesis test.
results_list_dgp2 <- parLapply(cl, 1:R, function(r) {
  # Generate synthetic data for replication r
  X_sim <- generate_DGP2(p, T, F_t)
  
  # Run the hypothesis test on the synthetic data
  test_result <- hyptest1(X_sim, m, B)
  
  # Return the relevant results as a list
  list(
    J_NT = test_result$J_NT,
    p_value = test_result$p_value,
    reject_H0 = test_result$p_value < 0.05
  )
})
# Shut down the cluster
stopCluster(cl)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Combine the list of results into a data frame
test_results_dgp2 <- do.call(rbind, lapply(results_list_dgp2, as.data.frame))

# Compute the rejection rate
rejection_rate <- mean(test_results_dgp2$reject_H0)

# Print summary statistics and a histogram of p-values
print(summary(test_results_dgp2$p_value))
hist(test_results_dgp2$p_value, breaks = 20, col = "lightblue", 
     main = "Histogram of p-values", xlab = "p-value", ylab = "Frequency", border = "black")

# Optionally, print the rejection rate
print(sprintf("Rejection rate: %.4f", rejection_rate))

##############
#    DGP3    #
##############

# Set up a cluster using all available cores
cl <- makeCluster(detectCores()-1)

# Export required functions and objects to the cluster's environment.
# Make sure to include all functions (generate_DGP1, hyptest1, etc.) 
# and objects (F_t, T, p, m, B) that will be used inside the worker function.
clusterExport(cl, varlist = c("generate_DGP3", "residuals","sqrt_matrix", "hyptest1", "compute_sigma_0", "silverman", "local_pca", "localPCA", "two_fold_convolution_kernel", "boundary_kernel", "compute_B_pT", "compute_M_hat", "compute_V_pT", "epanechnikov_kernel", "F_t", "T", "p", "m", "B", "R"))
clusterEvalQ(cl, {
  library(TVMVP)
  library(MASS)
})

start.time <- Sys.time()
# Run the simulation in parallel using parLapply.
# Each worker generates synthetic data and runs the hypothesis test.
results_list_dgp3 <- parLapply(cl, 1:R, function(r) {
  # Generate synthetic data for replication r
  X_sim <- generate_DGP3(p, T, F_t)
  
  # Run the hypothesis test on the synthetic data
  test_result <- hyptest1(X_sim, m, B)
  
  # Return the relevant results as a list
  list(
    J_NT = test_result$J_NT,
    p_value = test_result$p_value,
    reject_H0 = test_result$p_value < 0.05
  )
})
# Shut down the cluster
stopCluster(cl)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Combine the list of results into a data frame
test_results_dgp3 <- do.call(rbind, lapply(results_list_dgp3, as.data.frame))

# Compute the rejection rate
rejection_rate <- mean(test_results_dgp3$reject_H0)

# Print summary statistics and a histogram of p-values
print(summary(test_results_dgp3$p_value))
hist(test_results_dgp3$p_value, breaks = 20, col = "lightblue", 
     main = "Histogram of p-values", xlab = "p-value", ylab = "Frequency", border = "black")

# Optionally, print the rejection rate
print(sprintf("Rejection rate: %.4f", rejection_rate))

##############
#    DGP4    #
##############

# Set up a cluster using all available cores
cl <- makeCluster(detectCores()-1)

# Export required functions and objects to the cluster's environment.
# Make sure to include all functions (generate_DGP1, hyptest1, etc.) 
# and objects (F_t, T, p, m, B) that will be used inside the worker function.
clusterExport(cl, varlist = c("generate_DGP4", "residuals","sqrt_matrix", "hyptest1", "compute_sigma_0", "silverman", "local_pca", "localPCA", "two_fold_convolution_kernel", "boundary_kernel", "compute_B_pT", "compute_M_hat", "compute_V_pT", "epanechnikov_kernel", "F_t", "T", "p", "m", "B", "R"))
clusterEvalQ(cl, {
  library(TVMVP)
  library(MASS)
})

start.time <- Sys.time()
# Run the simulation in parallel using parLapply.
# Each worker generates synthetic data and runs the hypothesis test.
results_list_dgp4 <- parLapply(cl, 1:R, function(r) {
  # Generate synthetic data for replication r
  X_sim <- generate_DGP4(p, T, F_t, b=2)
  
  # Run the hypothesis test on the synthetic data
  test_result <- hyptest1(X_sim, m, B)
  
  # Return the relevant results as a list
  list(
    J_NT = test_result$J_NT,
    p_value = test_result$p_value,
    reject_H0 = test_result$p_value < 0.05
  )
})
# Shut down the cluster
stopCluster(cl)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Combine the list of results into a data frame
test_results_dgp4 <- do.call(rbind, lapply(results_list_dgp4, as.data.frame))

# Compute the rejection rate
rejection_rate <- mean(test_results_dgp4$reject_H0)

# Print summary statistics and a histogram of p-values
print(summary(test_results_dgp4$p_value))
hist(test_results_dgp4$p_value, breaks = 20, col = "lightblue", 
     main = "Histogram of p-values", xlab = "p-value", ylab = "Frequency", border = "black")

# Optionally, print the rejection rate
print(sprintf("Rejection rate: %.4f", rejection_rate))

##############
#    DGP5    #
##############

# Set up a cluster using all available cores
cl <- makeCluster(detectCores()-1)

# Export required functions and objects to the cluster's environment.
# Make sure to include all functions (generate_DGP1, hyptest1, etc.) 
# and objects (F_t, T, p, m, B) that will be used inside the worker function.
clusterExport(cl, varlist = c("generate_DGP5", "residuals","sqrt_matrix", "hyptest1", "compute_sigma_0", "silverman", "local_pca", "localPCA", "two_fold_convolution_kernel", "boundary_kernel", "compute_B_pT", "compute_M_hat", "compute_V_pT", "epanechnikov_kernel", "F_t", "T", "p", "m", "B", "R"))
clusterEvalQ(cl, {
  library(TVMVP)
  library(MASS)
})

start.time <- Sys.time()
# Run the simulation in parallel using parLapply.
# Each worker generates synthetic data and runs the hypothesis test.
results_list_dgp5 <- parLapply(cl, 1:R, function(r) {
  # Generate synthetic data for replication r
  X_sim <- generate_DGP5(p, T, F_t, b=2)
  
  # Run the hypothesis test on the synthetic data
  test_result <- hyptest1(X_sim, m, B)
  
  # Return the relevant results as a list
  list(
    J_NT = test_result$J_NT,
    p_value = test_result$p_value,
    reject_H0 = test_result$p_value < 0.05
  )
})
# Shut down the cluster
stopCluster(cl)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Combine the list of results into a data frame
test_results_dgp5 <- do.call(rbind, lapply(results_list_dgp5, as.data.frame))

# Compute the rejection rate
rejection_rate <- mean(test_results_dgp5$reject_H0)

# Print summary statistics and a histogram of p-values
print(summary(test_results_dgp5$p_value))
hist(test_results_dgp5$p_value, breaks = 20, col = "lightblue", 
     main = "Histogram of p-values", xlab = "p-value", ylab = "Frequency", border = "black")

# Optionally, print the rejection rate
print(sprintf("Rejection rate: %.4f", rejection_rate))

##############
#    DGP6    #
##############

# Set up a cluster using all available cores
cl <- makeCluster(detectCores()-1)

# Export required functions and objects to the cluster's environment.
# Make sure to include all functions (generate_DGP1, hyptest1, etc.) 
# and objects (F_t, T, p, m, B) that will be used inside the worker function.
clusterExport(cl, varlist = c("generate_DGP6", "residuals","sqrt_matrix", "hyptest1", "compute_sigma_0", "silverman", "local_pca", "localPCA", "two_fold_convolution_kernel", "boundary_kernel", "compute_B_pT", "compute_M_hat", "compute_V_pT", "epanechnikov_kernel", "F_t", "T", "p", "m", "B", "R"))
clusterEvalQ(cl, {
  library(TVMVP)
  library(MASS)
})

start.time <- Sys.time()
# Run the simulation in parallel using parLapply.
# Each worker generates synthetic data and runs the hypothesis test.
results_list_dgp6 <- parLapply(cl, 1:R, function(r) {
  # Generate synthetic data for replication r
  X_sim <- generate_DGP6(p, T, F_t, b=2)
  
  # Run the hypothesis test on the synthetic data
  test_result <- hyptest1(X_sim, m, B)
  
  # Return the relevant results as a list
  list(
    J_NT = test_result$J_NT,
    p_value = test_result$p_value,
    reject_H0 = test_result$p_value < 0.05
  )
})
# Shut down the cluster
stopCluster(cl)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Combine the list of results into a data frame
test_results_dgp6 <- do.call(rbind, lapply(results_list_dgp6, as.data.frame))

# Compute the rejection rate
rejection_rate <- mean(test_results_dgp6$reject_H0)

# Print summary statistics and a histogram of p-values
print(summary(test_results_dgp6$p_value))
hist(test_results_dgp6$p_value, breaks = 20, col = "lightblue", 
     main = "Histogram of p-values", xlab = "p-value", ylab = "Frequency", border = "black")

# Optionally, print the rejection rate
print(sprintf("Rejection rate: %.4f", rejection_rate))

