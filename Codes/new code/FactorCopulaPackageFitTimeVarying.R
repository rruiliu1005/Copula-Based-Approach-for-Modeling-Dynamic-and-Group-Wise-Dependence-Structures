package_times_non_parallel_gaussian = c()
################################################################################
start <- Sys.time()
period <- 90 
gaussian_results_90 <- list()
progress <- txtProgressBar(min = 0, max = (nrow(unif_df) - period), style = 3)

for (i in 1:(nrow(unif_df) - period)) {
  setTxtProgressBar(progress, i)  
  tmp <- unif_df[i:(i + period), ]
  
  out <- FactorCopula::mle1factor(continuous = tmp, copF1 = rep('bvn', ncol(tmp)), 
                                  gl = statmod::gauss.quad.prob(35), hessian = TRUE)
  
  gaussian_results_90[[i]] <- unlist(out$cpar$f1)
}

end <- Sys.time()
package_times_non_parallel_gaussian <- c(package_times_non_parallel_gaussian, as.numeric(difftime(end, start, units = "secs")))
close(progress) 

save(gaussian_results_90, file = "/Users/ruiliu/Desktop/research/results/gaussian_results_90_package.RData")
################################################################################
start <- Sys.time()
period <- 180
gaussian_results_180 <- list()
progress <- txtProgressBar(min = 0, max = (nrow(unif_df) - period), style = 3)

for (i in 1:(nrow(unif_df) - period)) {
  setTxtProgressBar(progress, i)  
  tmp <- unif_df[i:(i + period), ]
  
  out <- FactorCopula::mle1factor(continuous = tmp, copF1 = rep('bvn', ncol(tmp)), 
                                  gl = statmod::gauss.quad.prob(35), hessian = TRUE)
  
  gaussian_results_180[[i]] <- unlist(out$cpar$f1)
}

end <- Sys.time()
package_times_non_parallel_gaussian <- c(package_times_non_parallel_gaussian, as.numeric(difftime(end, start, units = "secs")))
close(progress)
save(gaussian_results_180, file = "/Users/ruiliu/Desktop/research/results/gaussian_results_180_package.RData")
################################################################################
start <- Sys.time()
period <- 360
gaussian_results_360 <- list()
progress <- txtProgressBar(min = 0, max = (nrow(unif_df) - period), style = 3)

for (i in 1:(nrow(unif_df) - period)) {
  setTxtProgressBar(progress, i)  
  tmp <- unif_df[i:(i + period), ]
  
  out <- FactorCopula::mle1factor(continuous = tmp, copF1 = rep('bvn', ncol(tmp)), 
                                  gl = statmod::gauss.quad.prob(35), hessian = TRUE)
  
  gaussian_results_360[[i]] <- unlist(out$cpar$f1)
}

end <- Sys.time()
package_times_non_parallel_gaussian <- c(package_times_non_parallel_gaussian, as.numeric(difftime(end, start, units = "secs")))
close(progress)
save(gaussian_results_360, file = "/Users/ruiliu/Desktop/research/results/gaussian_results_360_package.RData")
################################################################################
start <- Sys.time()
period <- 720
gaussian_results_720 <- list()
progress <- txtProgressBar(min = 0, max = (nrow(unif_df) - period), style = 3)

for (i in 1:(nrow(unif_df) - period)) {
  setTxtProgressBar(progress, i)  
  tmp <- unif_df[i:(i + period), ]
  
  out <- FactorCopula::mle1factor(continuous = tmp, copF1 = rep('bvn', ncol(tmp)), 
                                  gl = statmod::gauss.quad.prob(35), hessian = TRUE)
  
  gaussian_results_720[[i]] <- unlist(out$cpar$f1)
}

end <- Sys.time()
package_times_non_parallel_gaussian <- c(package_times_non_parallel_gaussian, as.numeric(difftime(end, start, units = "secs")))
close(progress)
save(gaussian_results_720, file = "/Users/ruiliu/Desktop/research/results/gaussian_results_720_package.RData")
save(package_times_non_parallel_gaussian, file = "/Users/ruiliu/Desktop/research/results/package_times_non_parallel_gaussian.RData")

################################################################################
################################################################################
package_times_non_parallel_gumbel = c()
################################################################################
start <- Sys.time()
period <- 90 
gumbel_results_90 <- list()
progress <- txtProgressBar(min = 0, max = (nrow(unif_df) - period), style = 3)

for (i in 1:(nrow(unif_df) - period)) {
  setTxtProgressBar(progress, i)  
  tmp <- unif_df[i:(i + period), ]
  
  out <- FactorCopula::mle1factor(continuous = tmp, copF1 = rep('gum', ncol(tmp)), 
                                  gl = statmod::gauss.quad.prob(35), hessian = TRUE)
  
  gumbel_results_90[[i]] <- unlist(out$cpar$f1)
}

end <- Sys.time()
package_times_non_parallel_gumbel <- c(package_times_non_parallel_gumbel, as.numeric(difftime(end, start, units = "secs")))
close(progress) 

save(gumbel_results_90, file = "/Users/ruiliu/Desktop/research/results/gumbel_results_90_package.RData")
################################################################################
start <- Sys.time()
period <- 180
gumbel_results_180 <- list()
progress <- txtProgressBar(min = 0, max = (nrow(unif_df) - period), style = 3)

for (i in 1:(nrow(unif_df) - period)) {
  setTxtProgressBar(progress, i)  
  tmp <- unif_df[i:(i + period), ]
  
  out <- FactorCopula::mle1factor(continuous = tmp, copF1 = rep('gum', ncol(tmp)), 
                                  gl = statmod::gauss.quad.prob(35), hessian = TRUE)
  
  gumbel_results_180[[i]] <- unlist(out$cpar$f1)
}

end <- Sys.time()
package_times_non_parallel_gumbel <- c(package_times_non_parallel_gumbel, as.numeric(difftime(end, start, units = "secs")))
close(progress)
save(gumbel_results_180, file = "/Users/ruiliu/Desktop/research/results/gumbel_results_180_package.RData")
################################################################################
start <- Sys.time()
period <- 360
gumbel_results_360 <- list()
progress <- txtProgressBar(min = 0, max = (nrow(unif_df) - period), style = 3)

for (i in 1:(nrow(unif_df) - period)) {
  setTxtProgressBar(progress, i)  
  tmp <- unif_df[i:(i + period), ]
  
  out <- FactorCopula::mle1factor(continuous = tmp, copF1 = rep('gum', ncol(tmp)), 
                                  gl = statmod::gauss.quad.prob(35), hessian = TRUE)
  
  gumbel_results_360[[i]] <- unlist(out$cpar$f1)
}

end <- Sys.time()
package_times_non_parallel_gumbel <- c(package_times_non_parallel_gumbel, as.numeric(difftime(end, start, units = "secs")))
close(progress)
save(gumbel_results_360, file = "/Users/ruiliu/Desktop/research/results/gumbel_results_360_package.RData")
################################################################################
start <- Sys.time()
period <- 720
gumbel_results_720 <- list()
progress <- txtProgressBar(min = 0, max = (nrow(unif_df) - period), style = 3)

for (i in 1:(nrow(unif_df) - period)) {
  setTxtProgressBar(progress, i)  
  tmp <- unif_df[i:(i + period), ]
  
  out <- FactorCopula::mle1factor(continuous = tmp, copF1 = rep('gum', ncol(tmp)), 
                                  gl = statmod::gauss.quad.prob(35), hessian = TRUE)
  
  gumbel_results_720[[i]] <- unlist(out$cpar$f1)
}

end <- Sys.time()
package_times_non_parallel_gumbel <- c(package_times_non_parallel_gumbel, as.numeric(difftime(end, start, units = "secs")))
close(progress)
save(gumbel_results_720, file = "/Users/ruiliu/Desktop/research/results/gumbel_results_720_package.RData")
save(package_times_non_parallel_gumbel, file = "/Users/ruiliu/Desktop/research/results/package_times_non_parallel_gumbel.RData")

################################################################################
################################################################################
package_times_non_parallel_rgumbel = c()
################################################################################
start <- Sys.time()
period <- 90 
rgumbel_results_90 <- list()
progress <- txtProgressBar(min = 0, max = (nrow(unif_df) - period), style = 3)

for (i in 1:(nrow(unif_df) - period)) {
  setTxtProgressBar(progress, i)  
  tmp <- unif_df[i:(i + period), ]
  
  out <- FactorCopula::mle1factor(continuous = tmp, copF1 = rep('rgum', ncol(tmp)), 
                                  gl = statmod::gauss.quad.prob(35), hessian = TRUE)
  
  rgumbel_results_90[[i]] <- unlist(out$cpar$f1)
}

end <- Sys.time()
package_times_non_parallel_rgumbel <- c(package_times_non_parallel_rgumbel, as.numeric(difftime(end, start, units = "secs")))
close(progress) 

save(rgumbel_results_90, file = "/Users/ruiliu/Desktop/research/results/rgumbel_results_90_package.RData")
################################################################################
start <- Sys.time()
period <- 180
rgumbel_results_180 <- list()
progress <- txtProgressBar(min = 0, max = (nrow(unif_df) - period), style = 3)

for (i in 1:(nrow(unif_df) - period)) {
  setTxtProgressBar(progress, i)  
  tmp <- unif_df[i:(i + period), ]
  
  out <- FactorCopula::mle1factor(continuous = tmp, copF1 = rep('rgum', ncol(tmp)), 
                                  gl = statmod::gauss.quad.prob(35), hessian = TRUE)
  
  rgumbel_results_180[[i]] <- unlist(out$cpar$f1)
}

end <- Sys.time()
package_times_non_parallel_rgumbel <- c(package_times_non_parallel_rgumbel, as.numeric(difftime(end, start, units = "secs")))
close(progress)
save(rgumbel_results_180, file = "/Users/ruiliu/Desktop/research/results/rgumbel_results_180_package.RData")
################################################################################
start <- Sys.time()
period <- 360
rgumbel_results_360 <- list()
progress <- txtProgressBar(min = 0, max = (nrow(unif_df) - period), style = 3)

for (i in 1:(nrow(unif_df) - period)) {
  setTxtProgressBar(progress, i)  
  tmp <- unif_df[i:(i + period), ]
  
  out <- FactorCopula::mle1factor(continuous = tmp, copF1 = rep('rgum', ncol(tmp)), 
                                  gl = statmod::gauss.quad.prob(35), hessian = TRUE)
  
  rgumbel_results_360[[i]] <- unlist(out$cpar$f1)
}

end <- Sys.time()
package_times_non_parallel_rgumbel <- c(package_times_non_parallel_rgumbel, as.numeric(difftime(end, start, units = "secs")))
close(progress)
save(rgumbel_results_360, file = "/Users/ruiliu/Desktop/research/results/rgumbel_results_360_package.RData")
################################################################################
start <- Sys.time()
period <- 720
rgumbel_results_720 <- list()
progress <- txtProgressBar(min = 0, max = (nrow(unif_df) - period), style = 3)

for (i in 1:(nrow(unif_df) - period)) {
  setTxtProgressBar(progress, i)  
  tmp <- unif_df[i:(i + period), ]
  
  out <- FactorCopula::mle1factor(continuous = tmp, copF1 = rep('rgum', ncol(tmp)), 
                                  gl = statmod::gauss.quad.prob(35), hessian = TRUE)
  
  rgumbel_results_720[[i]] <- unlist(out$cpar$f1)
}

end <- Sys.time()
package_times_non_parallel_rgumbel <- c(package_times_non_parallel_rgumbel, as.numeric(difftime(end, start, units = "secs")))
close(progress)
save(rgumbel_results_720, file = "/Users/ruiliu/Desktop/research/results/rgumbel_results_720_package.RData")
save(package_times_non_parallel_rgumbel, file = "/Users/ruiliu/Desktop/research/results/package_times_non_parallel_rgumbel.RData")


################################################################################
####################parallel processing#########################################
################################################################################


library(parallel)
library(pbapply)
library(FactorCopula)

# Main function for parallel computation
fit_gaussian_copula_time_varying <- function(df, rho, n_nodes, period){
  
  # Number of cores to use
  num_cores <- detectCores() - 1  # Leave one core free
  # Create cluster
  cl <- makeCluster(num_cores)
  
  # Define the task to be parallelized
  task <- function(i, df, period) {
    # Subset the data for the current window
    tmp <- df[i:(i + period), ]
    
    # Perform the copula estimation on the subset
    result_test <- FactorCopula::mle1factor(continuous = tmp, copF1 = rep('bvn', ncol(tmp)), gl = statmod::gauss.quad.prob(35), hessian = TRUE)
    
    # Return the results
    return(list(
      likelihood = result_test$loglik,
      estimate = unlist(result_test$cpar$f1)
    ))
  }
  
  # Use pblapply to handle progress bar and parallelization
  results <- pblapply(1:(nrow(df) - period), function(i) task(i, df, period), cl = cl)
  
  # Close the cluster
  stopCluster(cl)
  
  # Extract results into separate lists
  likelihoods <- sapply(results, function(res) res$likelihood)
  estimates <- lapply(results, function(res) res$estimate)
  
  return(list(likelihoods = likelihoods, estimates = estimates))
}

# Example run of the function
start <- Sys.time()

result_gaussian_90_days_package <- fit_gaussian_copula_time_varying(unif_df, runif(ncol(unif_df), min = -1, max = 1), 35, 90)
save(result_gaussian_90_days_package, file = "/Users/ruiliu/Desktop/research/results/result_gaussian_90_days_package.RData")
end <- Sys.time()
print(end - start)

# Example run of the function
start <- Sys.time()

result_gaussian_180_days_package <- fit_gaussian_copula_time_varying(unif_df, runif(ncol(unif_df), min = -1, max = 1), 35, 180)
save(result_gaussian_180_days_package, file = "/Users/ruiliu/Desktop/research/results/result_gaussian_180_days_package.RData")

end <- Sys.time()
print(end - start)

# Example run of the function
start <- Sys.time()

result_gaussian_360_days_package <- fit_gaussian_copula_time_varying(unif_df, runif(ncol(unif_df), min = -1, max = 1), 35, 360)
save(result_gaussian_360_days_package, file = "/Users/ruiliu/Desktop/research/results/result_gaussian_360_days_package.RData")

end <- Sys.time()
print(end - start)







# Main function for parallel computation
fit_gumbel_copula_time_varying <- function(df, rho, n_nodes, period){
  
  # Number of cores to use
  num_cores <- detectCores() - 1  # Leave one core free
  # Create cluster
  cl <- makeCluster(num_cores)
  
  # Define the task to be parallelized
  task <- function(i, df, period) {
    # Subset the data for the current window
    tmp <- df[i:(i + period), ]
    
    # Perform the copula estimation on the subset
    result_test <- FactorCopula::mle1factor(continuous = tmp, copF1 = rep('gum', ncol(tmp)), gl = statmod::gauss.quad.prob(35), hessian = TRUE)
    
    # Return the results
    return(list(
      likelihood = result_test$loglik,
      estimate = unlist(result_test$cpar$f1)
    ))
  }
  
  # Use pblapply to handle progress bar and parallelization
  results <- pblapply(1:(nrow(df) - period), function(i) task(i, df, period), cl = cl)
  
  # Close the cluster
  stopCluster(cl)
  
  # Extract results into separate lists
  likelihoods <- sapply(results, function(res) res$likelihood)
  estimates <- lapply(results, function(res) res$estimate)
  
  return(list(likelihoods = likelihoods, estimates = estimates))
}






# Example run of the function
start <- Sys.time()

result_gumbel_90_days_package <- fit_gumbel_copula_time_varying(unif_df, runif(ncol(unif_df), min = -1, max = 1), 35, 90)
save(result_gumbel_90_days_package, file = "/Users/ruiliu/Desktop/research/results/result_gumbel_90_days_package.RData")

end <- Sys.time()
print(end - start)

# Example run of the function
start <- Sys.time()

result_gumbel_180_days_package <- fit_gumbel_copula_time_varying(unif_df, runif(ncol(unif_df), min = -1, max = 1), 35, 180)
save(result_gumbel_180_days_package, file = "/Users/ruiliu/Desktop/research/results/result_gumbel_180_days_package.RData")

end <- Sys.time()
print(end - start)

# Example run of the function
start <- Sys.time()

result_gumbel_360_days_package <- fit_gumbel_copula_time_varying(unif_df, runif(ncol(unif_df), min = -1, max = 1), 35, 360)
save(result_gumbel_360_days_package, file = "/Users/ruiliu/Desktop/research/results/result_gumbel_360_days_package.RData")

end <- Sys.time()
print(end - start)


# Main function for parallel computation
fit_bb1_copula_time_varying <- function(df, rho, n_nodes, period){
  
  # Number of cores to use
  num_cores <- detectCores() - 1  # Leave one core free
  # Create cluster
  cl <- makeCluster(num_cores)
  
  # Define the task to be parallelized
  task <- function(i, df, period) {
    # Subset the data for the current window
    tmp <- df[i:(i + period), ]
    
    # Perform the copula estimation on the subset
    result_test <- FactorCopula::mle1factor(continuous = tmp, copF1 = rep('bb1', ncol(tmp)), gl = statmod::gauss.quad.prob(35), hessian = TRUE)
    
    # Return the results
    return(list(
      likelihood = result_test$loglik,
      estimate = unlist(result_test$cpar$f1)
    ))
  }
  
  # Use pblapply to handle progress bar and parallelization
  results <- pblapply(1:(nrow(df) - period), function(i) task(i, df, period), cl = cl)
  
  # Close the cluster
  stopCluster(cl)
  
  # Extract results into separate lists
  likelihoods <- sapply(results, function(res) res$likelihood)
  estimates <- lapply(results, function(res) res$estimate)
  
  return(list(likelihoods = likelihoods, estimates = estimates))
}






# Example run of the function
start <- Sys.time()

result_bb1_90_days_package <- fit_bb1_copula_time_varying(unif_df, runif(ncol(unif_df), min = -1, max = 1), 35, 90)
save(result_bb1_90_days_package, file = "/Users/ruiliu/Desktop/research/results/result_bb1_90_days_package.RData")

end <- Sys.time()
print(end - start)

# Example run of the function
start <- Sys.time()

result_bb1_180_days_package <- fit_bb1_copula_time_varying(unif_df, runif(ncol(unif_df), min = -1, max = 1), 35, 180)
save(result_bb1_180_days_package, file = "/Users/ruiliu/Desktop/research/results/result_bb1_180_days_package.RData")

end <- Sys.time()
print(end - start)

# Example run of the function
start <- Sys.time()

result_bb1_360_days_package <- fit_bb1_copula_time_varying(unif_df, runif(ncol(unif_df), min = -1, max = 1), 35, 360)
save(result_bb1_360_days_package, file = "/Users/ruiliu/Desktop/research/results/result_bb1_360_days_package.RData")

end <- Sys.time()
print(end - start)