library(stats)
library(pracma)
library(FactorCopula)
library(cubature)
library(copula)
library(matrixStats)
setwd('/Users/ruiliu/Desktop/research')
original_df = read.csv('./data/unif_df.csv')
unif_df = original_df[, 1:20]
head(unif_df)

group_1_1 = unif_df[, c("BTCUSDT", "ETHUSDT", "BNBUSDT", "XRPUSDT", "LTCUSDT")]
group_1_2 = unif_df[, c("ADAUSDT", "SOLUSDT", "DOTUSDT", "ATOMUSDT", "AVAXUSDT")]
group_1_3 = unif_df[, c("DOGEUSDT", "BCHUSDT", "TRXUSDT", "ETCUSDT", "MATICUSDT", "FTMUSDT", "FILUSDT", "NEARUSDT", "LINKUSDT", "EOSUSDT")]

group_2_1 = unif_df[, c("ETHUSDT", "SOLUSDT", "DOTUSDT", "ATOMUSDT", "AVAXUSDT", "MATICUSDT", "LINKUSDT")]
group_2_2 = unif_df[, c("BTCUSDT", "BNBUSDT", "XRPUSDT", "LTCUSDT", "BCHUSDT")]
group_2_3 = unif_df[, c("DOGEUSDT", "ADAUSDT", "TRXUSDT", "ETCUSDT", "EOSUSDT", "FTMUSDT", "FILUSDT", "NEARUSDT")]

library(parallel)
library(pbapply)

fit_gaussian_copula_time_varying <- function(df, rho, period){
  
  # Define necessary functions within the main function
  gaussian_derivative_single_obs <- function(u1, rho, nl_x2, wl){
    rho_squared <- rho^2
    one_minus_rho_squared <- 1 - rho_squared
    
    u1 = as.numeric(u1)
    x1 <- qnorm(u1, mean = 0, sd = 1)
    x2 = matrix(rep(nl_x2, 20), ncol = 35, nrow = 20, byrow = TRUE)
    
    numerator <- 2 * rho * x1 * x2 - rho_squared * (x1^2 + x2^2)
    denominator <- 2 * one_minus_rho_squared
    exponent <- numerator / denominator
    out <- exp(exponent) / sqrt(one_minus_rho_squared)
    prod_vec = apply(out, 2, prod)
    
    # Calculate numerator
    deriv_mul_vec = (rho * one_minus_rho_squared + (1 + rho_squared) * x1 * x2 - rho * (x1^2 + x2^2)) / (one_minus_rho_squared^2)
    num = deriv_mul_vec %*% diag(prod_vec) %*% diag(wl)
    num = rowSums(num)
    
    # Calculate denominator
    denom = sum(prod_vec * wl)
    result = num/denom
    return(list(log_likelihood = log(denom), derivatives = result))
  }
  
  gaussian_derivative <- function(df, rho, nl_x2, wl){
    out = t(apply(df, 1, gaussian_derivative_single_obs, rho = rho, nl_x2 = nl_x2, wl = wl))
    log_likelihood <- sum(sapply(out, function(x) x$log_likelihood))
    derivatives <- rowSums(sapply(out, function(x) x$derivatives))
    return(list(log_likelihood = log_likelihood, derivatives = derivatives))
  }
  
  neg_log_gaussian_likelihood <- function(df, rho, nl_x2, wl){
    print(rho[1])
    if ((max(rho) > 0.9999999) | (min(rho) < -0.9999999) | rho[1]<0){
      log_likelihood = 1e10
      attr(log_likelihood, "gradient") = 1e6*sign(rho)
      return(log_likelihood)
    }
    result = gaussian_derivative(df, rho, nl_x2, wl)
    log_likelihood = -1 * result$log_likelihood
    gradient = result$derivatives
    attr(log_likelihood, "gradient") = -1 * gradient
    return(log_likelihood)
  }
  
  # Number of cores to use
  num_cores <- detectCores() - 1  # Leave one core free
  
  # Create cluster
  cl <- makeCluster(num_cores)
  
  # Export necessary variables to worker nodes, including unif_df
  clusterExport(cl, varlist = c("df", "rho", "nl_x2", "wl", "neg_log_gaussian_likelihood", "gaussian_derivative", "gaussian_derivative_single_obs"))
  
  # Define the task to be parallelized
  task <- function(i) {
    tmp <- df[i:(i + period), ]
    result_test <- nlm(f = neg_log_gaussian_likelihood, p = rho, df = tmp, nl_x2 = nl_x2, wl = wl, gradtol = 1e-5, hessian = FALSE, check.analyticals = FALSE, steptol = 1e-10, iterlim = 10000)
    
    return(list(
      likelihood = result_test$minimum,
      estimate = result_test$estimate,
      gradient = result_test$gradient
    ))
  }
  
  # Parallel execution over chunks with progress tracking using pbapply
  results <- pblapply(1:(nrow(df) - period), task, cl = cl)
  
  stopCluster(cl)  # Close the cluster
  
  # Extract results into separate lists
  likelihoods <- sapply(results, function(res) res$likelihood)
  estimates <- lapply(results, function(res) res$estimate)
  gradients <- lapply(results, function(res) res$gradient)
  
  return(list(likelihoods = likelihoods, estimates = estimates, gradients = gradients))
}


gaussian_times = c()
n_nodes = 35
gl <- gaussLegendre(n_nodes, 0, 1)
nl_x2 <- qnorm(gl$x)
wl <- gl$w

start = Sys.time()
result_gaussian_90_days = fit_gaussian_copula_time_varying(group_1_1, rep(0.5, 5), 90)
end = Sys.time()
gaussian_times = c(gaussian_times, end - start)
print(end - start)
save(result_gaussian_90_days, file = "/Users/ruiliu/Desktop/research/results/group_1_1_result_gaussian_90_days.RData")

start = Sys.time()
result_gaussian_180_days = fit_gaussian_copula_time_varying(group_1_1, rep(0.5, 5), 180)
end = Sys.time()
gaussian_times = c(gaussian_times, end - start)
print(end - start)
save(result_gaussian_180_days, file = "/Users/ruiliu/Desktop/research/results/group_1_1_result_gaussian_180_days.RData")

start = Sys.time()
result_gaussian_360_days = fit_gaussian_copula_time_varying(group_1_1, rep(0.5, 5), 360)
end = Sys.time()
gaussian_times = c(gaussian_times, end - start)
print(end - start)
save(result_gaussian_360_days, file = "/Users/ruiliu/Desktop/research/results/group_1_1_result_gaussian_360_days.RData")

start = Sys.time()
result_gaussian_720_days = fit_gaussian_copula_time_varying(group_1_1, rep(0.5, 5), 720)
end = Sys.time()
gaussian_times = c(gaussian_times, end - start)
print(end - start)
save(result_gaussian_720_days, file = "/Users/ruiliu/Desktop/research/results/group_1_1_result_gaussian_720_days.RData")

save(gaussian_times, file = "/Users/ruiliu/Desktop/research/results/group_1_1_gaussian_time_varying_times.RData")



gaussian_times = c()
n_nodes = 35
gl <- gaussLegendre(n_nodes, 0, 1)
nl_x2 <- qnorm(gl$x)
wl <- gl$w

start = Sys.time()
result_gaussian_90_days = fit_gaussian_copula_time_varying(group_1_2, rep(0.5, 5), 90)
end = Sys.time()
gaussian_times = c(gaussian_times, end - start)
print(end - start)
save(result_gaussian_90_days, file = "/Users/ruiliu/Desktop/research/results/group_1_2_result_gaussian_90_days.RData")

start = Sys.time()
result_gaussian_180_days = fit_gaussian_copula_time_varying(group_1_2, rep(0.5, 5), 180)
end = Sys.time()
gaussian_times = c(gaussian_times, end - start)
print(end - start)
save(result_gaussian_180_days, file = "/Users/ruiliu/Desktop/research/results/group_1_2_result_gaussian_180_days.RData")

start = Sys.time()
result_gaussian_360_days = fit_gaussian_copula_time_varying(group_1_2, rep(0.5, 5), 360)
end = Sys.time()
gaussian_times = c(gaussian_times, end - start)
print(end - start)
save(result_gaussian_360_days, file = "/Users/ruiliu/Desktop/research/results/group_1_2_result_gaussian_360_days.RData")

start = Sys.time()
result_gaussian_720_days = fit_gaussian_copula_time_varying(group_1_2, rep(0.5, 5), 720)
end = Sys.time()
gaussian_times = c(gaussian_times, end - start)
print(end - start)
save(result_gaussian_720_days, file = "/Users/ruiliu/Desktop/research/results/group_1_2_result_gaussian_720_days.RData")

save(gaussian_times, file = "/Users/ruiliu/Desktop/research/results/group_1_2_gaussian_time_varying_times.RData")


gaussian_times = c()
n_nodes = 35
gl <- gaussLegendre(n_nodes, 0, 1)
nl_x2 <- qnorm(gl$x)
wl <- gl$w

start = Sys.time()
result_gaussian_90_days = fit_gaussian_copula_time_varying(group_1_3, rep(0.5, 10), 90)
end = Sys.time()
gaussian_times = c(gaussian_times, end - start)
print(end - start)
save(result_gaussian_90_days, file = "/Users/ruiliu/Desktop/research/results/group_1_3_result_gaussian_90_days.RData")

start = Sys.time()
result_gaussian_180_days = fit_gaussian_copula_time_varying(group_1_3, rep(0.5, 10), 180)
end = Sys.time()
gaussian_times = c(gaussian_times, end - start)
print(end - start)
save(result_gaussian_180_days, file = "/Users/ruiliu/Desktop/research/results/group_1_3_result_gaussian_180_days.RData")

start = Sys.time()
result_gaussian_360_days = fit_gaussian_copula_time_varying(group_1_3, rep(0.5, 10), 360)
end = Sys.time()
gaussian_times = c(gaussian_times, end - start)
print(end - start)
save(result_gaussian_360_days, file = "/Users/ruiliu/Desktop/research/results/group_1_3_result_gaussian_360_days.RData")

start = Sys.time()
result_gaussian_720_days = fit_gaussian_copula_time_varying(group_1_3, rep(0.5, 10), 720)
end = Sys.time()
gaussian_times = c(gaussian_times, end - start)
print(end - start)
save(result_gaussian_720_days, file = "/Users/ruiliu/Desktop/research/results/group_1_3_result_gaussian_720_days.RData")

save(gaussian_times, file = "/Users/ruiliu/Desktop/research/results/group_1_3_gaussian_time_varying_times.RData")


gaussian_times = c()
n_nodes = 35
gl <- gaussLegendre(n_nodes, 0, 1)
nl_x2 <- qnorm(gl$x)
wl <- gl$w

start = Sys.time()
result_gaussian_90_days = fit_gaussian_copula_time_varying(group_2_1, rep(0.5, 7), 90)
end = Sys.time()
gaussian_times = c(gaussian_times, end - start)
print(end - start)
save(result_gaussian_90_days, file = "/Users/ruiliu/Desktop/research/results/group_2_1_result_gaussian_90_days.RData")

start = Sys.time()
result_gaussian_180_days = fit_gaussian_copula_time_varying(group_2_1, rep(0.5, 7), 180)
end = Sys.time()
gaussian_times = c(gaussian_times, end - start)
print(end - start)
save(result_gaussian_180_days, file = "/Users/ruiliu/Desktop/research/results/group_2_1_result_gaussian_180_days.RData")

start = Sys.time()
result_gaussian_360_days = fit_gaussian_copula_time_varying(group_2_1, rep(0.5, 7), 360)
end = Sys.time()
gaussian_times = c(gaussian_times, end - start)
print(end - start)
save(result_gaussian_360_days, file = "/Users/ruiliu/Desktop/research/results/group_2_1_result_gaussian_360_days.RData")

start = Sys.time()
result_gaussian_720_days = fit_gaussian_copula_time_varying(group_2_1, rep(0.5, 7), 720)
end = Sys.time()
gaussian_times = c(gaussian_times, end - start)
print(end - start)
save(result_gaussian_720_days, file = "/Users/ruiliu/Desktop/research/results/group_2_1_result_gaussian_720_days.RData")

save(gaussian_times, file = "/Users/ruiliu/Desktop/research/results/group_2_1_gaussian_time_varying_times.RData")



gaussian_times = c()
n_nodes = 35
gl <- gaussLegendre(n_nodes, 0, 1)
nl_x2 <- qnorm(gl$x)
wl <- gl$w

start = Sys.time()
result_gaussian_90_days = fit_gaussian_copula_time_varying(group_2_2, rep(0.5, 5), 90)
end = Sys.time()
gaussian_times = c(gaussian_times, end - start)
print(end - start)
save(result_gaussian_90_days, file = "/Users/ruiliu/Desktop/research/results/group_2_2_result_gaussian_90_days.RData")

start = Sys.time()
result_gaussian_180_days = fit_gaussian_copula_time_varying(group_2_2, rep(0.5, 5), 180)
end = Sys.time()
gaussian_times = c(gaussian_times, end - start)
print(end - start)
save(result_gaussian_180_days, file = "/Users/ruiliu/Desktop/research/results/group_2_2_result_gaussian_180_days.RData")

start = Sys.time()
result_gaussian_360_days = fit_gaussian_copula_time_varying(group_2_2, rep(0.5, 5), 360)
end = Sys.time()
gaussian_times = c(gaussian_times, end - start)
print(end - start)
save(result_gaussian_360_days, file = "/Users/ruiliu/Desktop/research/results/group_2_2_result_gaussian_360_days.RData")

start = Sys.time()
result_gaussian_720_days = fit_gaussian_copula_time_varying(group_2_2, rep(0.5, 5), 720)
end = Sys.time()
gaussian_times = c(gaussian_times, end - start)
print(end - start)
save(result_gaussian_720_days, file = "/Users/ruiliu/Desktop/research/results/group_2_2_result_gaussian_720_days.RData")

save(gaussian_times, file = "/Users/ruiliu/Desktop/research/results/group_2_2_gaussian_time_varying_times.RData")


gaussian_times = c()
n_nodes = 35
gl <- gaussLegendre(n_nodes, 0, 1)
nl_x2 <- qnorm(gl$x)
wl <- gl$w

start = Sys.time()
result_gaussian_90_days = fit_gaussian_copula_time_varying(group_2_3, rep(0.5, 8), 90)
end = Sys.time()
gaussian_times = c(gaussian_times, end - start)
print(end - start)
save(result_gaussian_90_days, file = "/Users/ruiliu/Desktop/research/results/group_2_3_result_gaussian_90_days.RData")

start = Sys.time()
result_gaussian_180_days = fit_gaussian_copula_time_varying(group_2_3, rep(0.5, 8), 180)
end = Sys.time()
gaussian_times = c(gaussian_times, end - start)
print(end - start)
save(result_gaussian_180_days, file = "/Users/ruiliu/Desktop/research/results/group_2_3_result_gaussian_180_days.RData")

start = Sys.time()
result_gaussian_360_days = fit_gaussian_copula_time_varying(group_2_3, rep(0.5, 8), 360)
end = Sys.time()
gaussian_times = c(gaussian_times, end - start)
save(result_gaussian_360_days, file = "/Users/ruiliu/Desktop/research/results/group_2_3_result_gaussian_360_days.RData")

start = Sys.time()
result_gaussian_720_days = fit_gaussian_copula_time_varying(group_2_3, rep(0.5, 8), 720)
end = Sys.time()
gaussian_times = c(gaussian_times, end - start)
print(end - start)
save(result_gaussian_720_days, file = "/Users/ruiliu/Desktop/research/results/group_2_3_result_gaussian_720_days.RData")

save(gaussian_times, file = "/Users/ruiliu/Desktop/research/results/group_2_3_gaussian_time_varying_times.RData")




fit_clayton_copula_time_varying <- function(df, theta, period, nl_u2) {
  
  # Number of cores to use
  num_cores <- detectCores() - 1  # Leave one core free
  
  # Create cluster
  cl <- makeCluster(num_cores)
  
  # Ensure all libraries are available in worker nodes
  clusterEvalQ(cl, {
    library(parallel)
    library(pbapply)
  })
  clusterExport(cl, varlist = c("nl_u2", "wl"))
  
  # Define the task to be parallelized
  task <- function(i, df, theta, period, nl_u2) {
    
    # Define necessary functions within the task
    clayton_single_obs_pre <- function(u1, u2, theta, d) {
      u1_theta = u1^(-theta)
      u2_theta = u2^(-theta)
      log_u1 = log(u1)
      log_u2 = log(u2)
      log_u1u2 = log(u1 * u2)
      g_theta = u1_theta + u2_theta - 1
      A = (theta + 1)
      B = g_theta^(-2 - 1 / theta)
      C = (u1 * u2)^(-theta - 1)
      
      B_deriv = B * ((1 / theta^2) * log(g_theta) + 
                       (-2 - 1 / theta) * (-u1_theta * log_u1 - u2_theta * log_u2) / g_theta)
      C_deriv = -C * log_u1u2
      
      bivariate_copula_values = A * B * C
      bivariate_copula_derivatives = B * C + A * (B_deriv * C + B * C_deriv)
      
      multiplication_mat = matrix(bivariate_copula_values, nrow = d, ncol = d)
      diag(multiplication_mat) = bivariate_copula_derivatives
      
      numerator = apply(multiplication_mat, 2, prod)
      denominator = prod(bivariate_copula_values)
      
      return(list("numerator" = numerator, "denominator" = denominator))
    }
    
    clayton_single_obs <- function(u1, theta, d, n_nodes, nl_u2, wl) {
      tmp = mapply(clayton_single_obs_pre, u2 = nl_u2, MoreArgs = list(u1 = u1, theta = theta, d))
      numerator_matrix = do.call(rbind, tmp[1, ])
      numerator = apply(numerator_matrix * wl, 2, sum)
      denominator = sum(unlist(tmp[2, ]) * wl)
      
      deriv_log_likelihood = numerator / denominator
      log_likelihood = log(denominator)
      return(list(log_likelihood = log_likelihood, deriv_log_likelihood = deriv_log_likelihood))
    }
    
    neg_clayton_loglikelihood <- function(df, theta, d, nl_u2, wl) {
      if (min(theta) < 1) {
        log_likelihood = 9e5
        attr(log_likelihood, "gradient") = 1e6 * sign(theta)
        return(log_likelihood)
      }
      out = t(apply(df, 1, clayton_single_obs, theta = theta, d = d, nl_u2 = nl_u2, wl = wl))
      log_likelihood <- sum(sapply(out, function(x) x$log_likelihood))
      derivatives <- rowSums(sapply(out, function(x) x$deriv_log_likelihood))
      
      neg_log_likelihood = -1 * log_likelihood
      attr(neg_log_likelihood, "gradient") = -1 * derivatives
      return(neg_log_likelihood)
    }
    
    # Main task logic
    tmp <- df[i:(i + period), ]
    result_test <- nlm(f = neg_clayton_loglikelihood, p = theta, df = tmp, d = ncol(df), nl_u2 = nl_u2, wl = wl, gradtol = 1e-2, hessian = FALSE, check.analyticals = FALSE, steptol = 1e-10, iterlim = 10000)
    
    return(list(
      likelihood = result_test$minimum,
      estimate = result_test$estimate,
      gradient = result_test$gradient
    ))
  }
  
  # Parallel execution over chunks with progress tracking using pbapply
  results <- pblapply(1:(nrow(df) - period), function(i) task(i, df, theta, period, nl_u2), cl = cl)
  
  stopCluster(cl)  # Close the cluster
  
  # Extract results into separate lists
  likelihoods <- sapply(results, function(res) res$likelihood)
  estimates <- lapply(results, function(res) res$estimate)
  gradients <- lapply(results, function(res) res$gradient)
  
  return(list(likelihoods = likelihoods, estimates = estimates, gradients = gradients))
}



clayton_times = c()
n_nodes = 35
gl <- gaussLegendre(n_nodes, 0, 1)
nl_u2 <- gl$x
wl <- gl$w

start = Sys.time()
result_clayton_90_days = fit_clayton_copula_time_varying(df = group_1_1, theta = rep(2, 5),period = 90, nl_u2 = nl_u2)
end = Sys.time()
clayton_times = c(clayton_times, end - start)
print(end - start)
save(result_clayton_90_days, file = "/Users/ruiliu/Desktop/research/results/group_1_1_result_clayton_90_days.RData")


start = Sys.time()
result_clayton_180_days = fit_clayton_copula_time_varying(df = group_1_1, theta = rep(2, 5),period = 180, nl_u2 = nl_u2)
end = Sys.time()
clayton_times = c(clayton_times, end - start)
print(end - start)
save(result_clayton_180_days, file = "/Users/ruiliu/Desktop/research/results/group_1_1_result_clayton_180_days.RData")

start = Sys.time()
result_clayton_360_days = fit_clayton_copula_time_varying(df = group_1_1, theta = rep(2, 5),period = 360, nl_u2 = nl_u2)
end = Sys.time()
clayton_times = c(clayton_times, end - start)
print(end - start)
save(result_clayton_360_days, file = "/Users/ruiliu/Desktop/research/results/group_1_1_result_clayton_360_days.RData")

start = Sys.time()
result_clayton_720_days = fit_clayton_copula_time_varying(df = group_1_1, theta = rep(2, 5),period = 720, nl_u2 = nl_u2)
end = Sys.time()
clayton_times = c(clayton_times, end - start)
print(end - start)
save(result_clayton_720_days, file = "/Users/ruiliu/Desktop/research/results/group_1_1_result_clayton_720_days.RData")

save(clayton_times, file = "/Users/ruiliu/Desktop/research/results/group_1_1_clayton_time_varying_times.RData")


clayton_times = c()
n_nodes = 35
gl <- gaussLegendre(n_nodes, 0, 1)
nl_u2 <- gl$x
wl <- gl$w

start = Sys.time()
result_clayton_90_days = fit_clayton_copula_time_varying(df = group_1_2, theta = rep(2, 5),period = 90, nl_u2 = nl_u2)
end = Sys.time()
clayton_times = c(clayton_times, end - start)
print(end - start)
save(result_clayton_90_days, file = "/Users/ruiliu/Desktop/research/results/group_1_2_result_clayton_90_days.RData")


start = Sys.time()
result_clayton_180_days = fit_clayton_copula_time_varying(df = group_1_2, theta = rep(2, 5),period = 180, nl_u2 = nl_u2)
end = Sys.time()
clayton_times = c(clayton_times, end - start)
print(end - start)
save(result_clayton_180_days, file = "/Users/ruiliu/Desktop/research/results/group_1_2_result_clayton_180_days.RData")

start = Sys.time()
result_clayton_360_days = fit_clayton_copula_time_varying(df = group_1_2, theta = rep(2, 5),period = 360, nl_u2 = nl_u2)
end = Sys.time()
clayton_times = c(clayton_times, end - start)
print(end - start)
save(result_clayton_360_days, file = "/Users/ruiliu/Desktop/research/results/group_1_2_result_clayton_360_days.RData")

start = Sys.time()
result_clayton_720_days = fit_clayton_copula_time_varying(df = group_1_2, theta = rep(2, 5),period = 720, nl_u2 = nl_u2)
end = Sys.time()
clayton_times = c(clayton_times, end - start)
print(end - start)
save(result_clayton_720_days, file = "/Users/ruiliu/Desktop/research/results/group_1_2_result_clayton_720_days.RData")

save(clayton_times, file = "/Users/ruiliu/Desktop/research/results/clayton_time_varying_times.RData")


clayton_times = c()
n_nodes = 35
gl <- gaussLegendre(n_nodes, 0, 1)
nl_u2 <- gl$x
wl <- gl$w

start = Sys.time()
result_clayton_90_days = fit_clayton_copula_time_varying(df = group_1_3, theta = rep(2, 10),period = 90, nl_u2 = nl_u2)
end = Sys.time()
clayton_times = c(clayton_times, end - start)
print(end - start)
save(result_clayton_90_days, file = "/Users/ruiliu/Desktop/research/results/group_1_3_result_clayton_90_days.RData")


start = Sys.time()
result_clayton_180_days = fit_clayton_copula_time_varying(df = group_1_3, theta = rep(2, 10),period = 180, nl_u2 = nl_u2)
end = Sys.time()
clayton_times = c(clayton_times, end - start)
print(end - start)
save(result_clayton_180_days, file = "/Users/ruiliu/Desktop/research/results/group_1_3_result_clayton_180_days.RData")

start = Sys.time()
result_clayton_360_days = fit_clayton_copula_time_varying(df = group_1_3, theta = rep(2, 10),period = 360, nl_u2 = nl_u2)
end = Sys.time()
clayton_times = c(clayton_times, end - start)
print(end - start)
save(result_clayton_360_days, file = "/Users/ruiliu/Desktop/research/results/group_1_3_result_clayton_360_days.RData")

start = Sys.time()
result_clayton_720_days = fit_clayton_copula_time_varying(df = group_1_3, theta = rep(2, 10),period = 720, nl_u2 = nl_u2)
end = Sys.time()
clayton_times = c(clayton_times, end - start)
print(end - start)
save(result_clayton_720_days, file = "/Users/ruiliu/Desktop/research/results/group_1_3_result_clayton_720_days.RData")

save(clayton_times, file = "/Users/ruiliu/Desktop/research/results/group_1_3_clayton_time_varying_times.RData")


clayton_times = c()
n_nodes = 35
gl <- gaussLegendre(n_nodes, 0, 1)
nl_u2 <- gl$x
wl <- gl$w

start = Sys.time()
result_clayton_90_days = fit_clayton_copula_time_varying(df = group_2_1, theta = rep(2, 7),period = 90, nl_u2 = nl_u2)
end = Sys.time()
clayton_times = c(clayton_times, end - start)
print(end - start)
save(result_clayton_90_days, file = "/Users/ruiliu/Desktop/research/results/group_2_1_result_clayton_90_days.RData")


start = Sys.time()
result_clayton_180_days = fit_clayton_copula_time_varying(df = group_2_1, theta = rep(2, 7),period = 180, nl_u2 = nl_u2)
end = Sys.time()
clayton_times = c(clayton_times, end - start)
print(end - start)
save(result_clayton_180_days, file = "/Users/ruiliu/Desktop/research/results/group_2_1_result_clayton_180_days.RData")

start = Sys.time()
result_clayton_360_days = fit_clayton_copula_time_varying(df = group_2_1, theta = rep(2, 7),period = 360, nl_u2 = nl_u2)
end = Sys.time()
clayton_times = c(clayton_times, end - start)
print(end - start)
save(result_clayton_360_days, file = "/Users/ruiliu/Desktop/research/results/group_2_1_result_clayton_360_days.RData")

start = Sys.time()
result_clayton_720_days = fit_clayton_copula_time_varying(df = group_2_1, theta = rep(2, 7),period = 720, nl_u2 = nl_u2)
end = Sys.time()
clayton_times = c(clayton_times, end - start)
print(end - start)
save(result_clayton_720_days, file = "/Users/ruiliu/Desktop/research/results/group_2_1_result_clayton_720_days.RData")

save(clayton_times, file = "/Users/ruiliu/Desktop/research/results/group_2_1_clayton_time_varying_times.RData")

clayton_times = c()
n_nodes = 35
gl <- gaussLegendre(n_nodes, 0, 1)
nl_u2 <- gl$x
wl <- gl$w

start = Sys.time()
result_clayton_90_days = fit_clayton_copula_time_varying(df = group_2_2, theta = rep(2, 5),period = 90, nl_u2 = nl_u2)
end = Sys.time()
clayton_times = c(clayton_times, end - start)
print(end - start)
save(result_clayton_90_days, file = "/Users/ruiliu/Desktop/research/results/group_2_2_result_clayton_90_days.RData")


start = Sys.time()
result_clayton_180_days = fit_clayton_copula_time_varying(df = group_2_2, theta = rep(2, 5),period = 180, nl_u2 = nl_u2)
end = Sys.time()
clayton_times = c(clayton_times, end - start)
print(end - start)
save(result_clayton_180_days, file = "/Users/ruiliu/Desktop/research/results/group_2_2_result_clayton_180_days.RData")

start = Sys.time()
result_clayton_360_days = fit_clayton_copula_time_varying(df = group_2_2, theta = rep(2, 5),period = 360, nl_u2 = nl_u2)
end = Sys.time()
clayton_times = c(clayton_times, end - start)
print(end - start)
save(result_clayton_360_days, file = "/Users/ruiliu/Desktop/research/results/group_2_2_result_clayton_360_days.RData")

start = Sys.time()
result_clayton_720_days = fit_clayton_copula_time_varying(df = group_2_2, theta = rep(2, 5),period = 720, nl_u2 = nl_u2)
end = Sys.time()
clayton_times = c(clayton_times, end - start)
print(end - start)
save(result_clayton_720_days, file = "/Users/ruiliu/Desktop/research/results/group_2_2_result_clayton_720_days.RData")

save(clayton_times, file = "/Users/ruiliu/Desktop/research/results/group_2_2_clayton_time_varying_times.RData")


clayton_times = c()
n_nodes = 35
gl <- gaussLegendre(n_nodes, 0, 1)
nl_u2 <- gl$x
wl <- gl$w

start = Sys.time()
result_clayton_90_days = fit_clayton_copula_time_varying(df = group_2_3, theta = rep(2, 8),period = 90, nl_u2 = nl_u2)
end = Sys.time()
clayton_times = c(clayton_times, end - start)
print(end - start)
save(result_clayton_90_days, file = "/Users/ruiliu/Desktop/research/results/group_2_3_result_clayton_90_days.RData")


start = Sys.time()
result_clayton_180_days = fit_clayton_copula_time_varying(df = group_2_3, theta = rep(2, 8),period = 180, nl_u2 = nl_u2)
end = Sys.time()
clayton_times = c(clayton_times, end - start)
print(end - start)
save(result_clayton_180_days, file = "/Users/ruiliu/Desktop/research/results/group_2_3_result_clayton_180_days.RData")

start = Sys.time()
result_clayton_360_days = fit_clayton_copula_time_varying(df = group_2_3, theta = rep(2, 8),period = 360, nl_u2 = nl_u2)
end = Sys.time()
clayton_times = c(clayton_times, end - start)
print(end - start)
save(result_clayton_360_days, file = "/Users/ruiliu/Desktop/research/results/group_2_3_result_clayton_360_days.RData")

start = Sys.time()
result_clayton_720_days = fit_clayton_copula_time_varying(df = group_2_3, theta = rep(2, 8),period = 720, nl_u2 = nl_u2)
end = Sys.time()
clayton_times = c(clayton_times, end - start)
print(end - start)
save(result_clayton_720_days, file = "/Users/ruiliu/Desktop/research/results/group_2_3_result_clayton_720_days.RData")

save(clayton_times, file = "/Users/ruiliu/Desktop/research/results/group_2_3_clayton_time_varying_times.RData")


fit_gumbel_copula_time_varying <- function(df, theta, period, nl_u2, n_nodes) {
  
  # Number of cores to use
  num_cores <- detectCores() - 1  # Leave one core free
  
  # Create cluster
  cl <- makeCluster(num_cores)
  
  # Ensure all libraries are available in worker nodes
  clusterEvalQ(cl, {
    library(parallel)
    library(pbapply)
    library(pracma)
  })
  clusterExport(cl, varlist = c("wl", "n_nodes"))
  
  # Define the task to be parallelized
  task <- function(i, df, theta, period, nl_u2) {
    
    # Define necessary functions within the task
    bivariate_gumbel_copula = function(u1, u2, theta){
      ln_u1 = log(u1)
      ln_u2 = log(u2)
      ln_mimus_ln_u1 = log(-ln_u1)
      ln_mimus_ln_u2 = log(-ln_u2)
      
      one_over_theta = 1/theta
      a_theta = (-ln_u1)^theta 
      b_theta = (-ln_u2)^theta
      A = (a_theta + b_theta)^one_over_theta
      B = A + theta - 1
      C = A^(1-2*theta)
      D = exp(-A)
      E = a_theta/ln_u1
      FF = b_theta/ln_u2
      G = (u1*u2)^-1
      out = B*C*D*E*FF*G
      
      deriv_A = A * ((a_theta * ln_mimus_ln_u1 + b_theta * ln_mimus_ln_u2)/(a_theta + b_theta) * one_over_theta - one_over_theta^2*log(a_theta + b_theta))
      deriv_B = deriv_A + 1 
      deriv_C = C * (deriv_A/A * (1-2*theta) - 2*log(A))
      deriv_D = D * (-deriv_A)
      deriv_E = E*(ln_mimus_ln_u1)
      deriv_F = FF*(ln_mimus_ln_u2)
      
      result = deriv_B * C * D * E * FF * G + B * deriv_C * D * E * FF * G + B * C * deriv_D * E * FF * G + B * C * D * deriv_E * FF * G + B * C * D * E * deriv_F * G 
      return(list(likelihood = out, derivative = result))
    }
    
    gumbel_single_obs_pre = function(u1, u2, theta, d){
      tmp =  mapply(bivariate_gumbel_copula, u1 = u1, theta = theta, MoreArgs = list(u2 = u2))
      bivariate_copula_values = unlist(tmp[1,])
      bivariate_copula_derivatives = unlist(tmp[2,])
      
      multiplication_mat = matrix(bivariate_copula_values, nrow = d, ncol = d)
      diag(multiplication_mat) = bivariate_copula_derivatives
      
      numerator = apply(multiplication_mat, 2, prod)
      denominator = prod(bivariate_copula_values)
      return(list("numerator" = numerator, "denominator" = denominator))
    }
    
    gumbel_single_obs = function(u1, theta, d, n_nodes){
      u1 = as.numeric(u1)
      gl = gaussLegendre(n = n_nodes, a = 0, b = 1)
      nl_u2 = gl$x
      wl = gl$w
      tmp = mapply(gumbel_single_obs_pre, u2 = nl_u2, MoreArgs = list(u1 = u1, theta = theta, d))
      numerator_matrix = do.call(rbind, tmp[1,])
      
      numerator = apply(numerator_matrix * wl, 2, sum)
      denominator = sum(unlist(tmp[2,])*wl)
      
      deriv_log_likelihood = numerator / denominator
      log_likelihood = log(denominator)
      # attr(log_likelihood, "gradient") = deriv_log_likelihood
      # return(log_likelihood)
      return(list(log_likelihood = log_likelihood, deriv_log_likelihood = deriv_log_likelihood))
    }
    
    
    neg_gumbel_loglikelihood = function(df, theta, d, n_nodes){
      if (min(theta) < 1){
        log_likelihood = 9e5
        attr(log_likelihood, "gradient") = 1e6*sign(theta)
        return(log_likelihood)
      }
      out = t(apply(df, 1, gumbel_single_obs, theta = theta, d = d, n_nodes = n_nodes))
      log_likelihood <- sum(sapply(out, function(x) x$log_likelihood))
      derivatives <- rowSums(sapply(out, function(x) x$deriv_log_likelihood))
      
      neg_log_likelihood = -1 * log_likelihood
      attr(neg_log_likelihood, "gradient") = -1 * derivatives
      return(neg_log_likelihood)
    }
    
    # Main task logic
    tmp <- df[i:(i + period), ]
    result_test <- nlm(f = neg_gumbel_loglikelihood, p = theta, df = tmp, d = ncol(df), n_nodes = n_nodes, gradtol = 1e-2, hessian = FALSE, check.analyticals = FALSE, steptol = 1e-10, iterlim = 10000)
    
    return(list(
      likelihood = result_test$minimum,
      estimate = result_test$estimate,
      gradient = result_test$gradient
    ))
  }
  
  # Parallel execution over chunks with progress tracking using pbapply
  results <- pblapply(1:(nrow(df) - period), function(i) task(i, df, theta, period, n_nodes), cl = cl)
  
  stopCluster(cl)  # Close the cluster
  
  # Extract results into separate lists
  likelihoods <- sapply(results, function(res) res$likelihood)
  estimates <- lapply(results, function(res) res$estimate)
  gradients <- lapply(results, function(res) res$gradient)
  
  return(list(likelihoods = likelihoods, estimates = estimates, gradients = gradients))
}


gumbel_times = c()
n_nodes = 35

start = Sys.time()
result_gumbel_90_days = fit_gumbel_copula_time_varying(df = group_1_1, theta = rep(2, 5), period = 90, n_nodes = n_nodes)
end = Sys.time()
gumbel_times = c(gumbel_times, end - start)
print(end - start)
save(result_gumbel_90_days, file = "/Users/ruiliu/Desktop/research/results/group_1_1_result_gumbel_90_days.RData")

start = Sys.time()
result_gumbel_180_days = fit_gumbel_copula_time_varying(df = group_1_1, theta = rep(2, 5), period = 180, n_nodes = n_nodes)
end = Sys.time()
gumbel_times = c(gumbel_times, end - start)
print(end - start)
save(result_gumbel_180_days, file = "/Users/ruiliu/Desktop/research/results/group_1_1_result_gumbel_180_days.RData")

start = Sys.time()
result_gumbel_360_days = fit_gumbel_copula_time_varying(df = group_1_1, theta = rep(2, 5), period = 360, n_nodes = n_nodes)
end = Sys.time()
gumbel_times = c(gumbel_times, end - start)
print(end - start)
save(result_gumbel_360_days, file = "/Users/ruiliu/Desktop/research/results/group_1_1_result_gumbel_360_days.RData")

start = Sys.time()
result_gumbel_720_days = fit_gumbel_copula_time_varying(df = unif_df, theta = rep(2, 5), period = 720, n_nodes = n_nodes)
end = Sys.time()
gumbel_times = c(gumbel_times, end - start)
print(end - start)
save(result_gumbel_720_days, file = "/Users/ruiliu/Desktop/research/results/group_1_1_result_gumbel_720_days.RData")

save(gumbel_times, file = "/Users/ruiliu/Desktop/research/results/group_1_1_gumbel_time_varying_times.RData")



gumbel_times = c()
n_nodes = 35

start = Sys.time()
result_gumbel_90_days = fit_gumbel_copula_time_varying(df = group_1_2, theta = rep(2, 5), period = 90, n_nodes = n_nodes)
end = Sys.time()
gumbel_times = c(gumbel_times, end - start)
print(end - start)
save(result_gumbel_90_days, file = "/Users/ruiliu/Desktop/research/results/group_1_2_result_gumbel_90_days.RData")

start = Sys.time()
result_gumbel_180_days = fit_gumbel_copula_time_varying(df = group_1_2, theta = rep(2, 5), period = 180, n_nodes = n_nodes)
end = Sys.time()
gumbel_times = c(gumbel_times, end - start)
print(end - start)
save(result_gumbel_180_days, file = "/Users/ruiliu/Desktop/research/results/group_1_2_result_gumbel_180_days.RData")

start = Sys.time()
result_gumbel_360_days = fit_gumbel_copula_time_varying(df = group_1_2, theta = rep(2, 5), period = 360, n_nodes = n_nodes)
end = Sys.time()
gumbel_times = c(gumbel_times, end - start)
print(end - start)
save(result_gumbel_360_days, file = "/Users/ruiliu/Desktop/research/results/group_1_2_result_gumbel_360_days.RData")

start = Sys.time()
result_gumbel_720_days = fit_gumbel_copula_time_varying(df = group_1_2, theta = rep(2, 5), period = 720, n_nodes = n_nodes)
end = Sys.time()
gumbel_times = c(gumbel_times, end - start)
print(end - start)
save(result_gumbel_720_days, file = "/Users/ruiliu/Desktop/research/results/group_1_2_result_gumbel_720_days.RData")

save(gumbel_times, file = "/Users/ruiliu/Desktop/research/results/group_1_2_gumbel_time_varying_times.RData")


gumbel_times = c()
n_nodes = 35

start = Sys.time()
result_gumbel_90_days = fit_gumbel_copula_time_varying(df = group_1_3, theta = rep(2, 10), period = 90, n_nodes = n_nodes)
end = Sys.time()
gumbel_times = c(gumbel_times, end - start)
print(end - start)
save(result_gumbel_90_days, file = "/Users/ruiliu/Desktop/research/results/group_1_3_result_gumbel_90_days.RData")

start = Sys.time()
result_gumbel_180_days = fit_gumbel_copula_time_varying(df = group_1_3, theta = rep(2, 10), period = 180, n_nodes = n_nodes)
end = Sys.time()
gumbel_times = c(gumbel_times, end - start)
print(end - start)
save(result_gumbel_180_days, file = "/Users/ruiliu/Desktop/research/results/group_1_3_result_gumbel_180_days.RData")

start = Sys.time()
result_gumbel_360_days = fit_gumbel_copula_time_varying(df = group_1_3, theta = rep(2, 10), period = 360, n_nodes = n_nodes)
end = Sys.time()
gumbel_times = c(gumbel_times, end - start)
print(end - start)
save(result_gumbel_360_days, file = "/Users/ruiliu/Desktop/research/results/group_1_3_result_gumbel_360_days.RData")

start = Sys.time()
result_gumbel_720_days = fit_gumbel_copula_time_varying(df = group_1_3, theta = rep(2, 10), period = 720, n_nodes = n_nodes)
end = Sys.time()
gumbel_times = c(gumbel_times, end - start)
print(end - start)
save(result_gumbel_720_days, file = "/Users/ruiliu/Desktop/research/results/group_1_3_result_gumbel_720_days.RData")

save(gumbel_times, file = "/Users/ruiliu/Desktop/research/results/group_1_3_gumbel_time_varying_times.RData")

gumbel_times = c()
n_nodes = 35

start = Sys.time()
result_gumbel_90_days = fit_gumbel_copula_time_varying(df = group_2_1, theta = rep(2, 7), period = 90, n_nodes = n_nodes)
end = Sys.time()
gumbel_times = c(gumbel_times, end - start)
print(end - start)
save(result_gumbel_90_days, file = "/Users/ruiliu/Desktop/research/results/group_2_1_result_gumbel_90_days.RData")

start = Sys.time()
result_gumbel_180_days = fit_gumbel_copula_time_varying(df = group_2_1, theta = rep(2, 7), period = 180, n_nodes = n_nodes)
end = Sys.time()
gumbel_times = c(gumbel_times, end - start)
print(end - start)
save(result_gumbel_180_days, file = "/Users/ruiliu/Desktop/research/results/group_2_1_result_gumbel_180_days.RData")

start = Sys.time()
result_gumbel_360_days = fit_gumbel_copula_time_varying(df = group_2_1, theta = rep(2, 7), period = 360, n_nodes = n_nodes)
end = Sys.time()
gumbel_times = c(gumbel_times, end - start)
print(end - start)
save(result_gumbel_360_days, file = "/Users/ruiliu/Desktop/research/results/group_2_1_result_gumbel_360_days.RData")

start = Sys.time()
result_gumbel_720_days = fit_gumbel_copula_time_varying(df = group_2_1, theta = rep(2, 7), period = 720, n_nodes = n_nodes)
end = Sys.time()
gumbel_times = c(gumbel_times, end - start)
print(end - start)
save(result_gumbel_720_days, file = "/Users/ruiliu/Desktop/research/results/group_2_1_result_gumbel_720_days.RData")

save(gumbel_times, file = "/Users/ruiliu/Desktop/research/results/group_2_1_gumbel_time_varying_times.RData")


gumbel_times = c()
n_nodes = 35

start = Sys.time()
result_gumbel_90_days = fit_gumbel_copula_time_varying(df = group_2_2, theta = rep(2, 5), period = 90, n_nodes = n_nodes)
end = Sys.time()
gumbel_times = c(gumbel_times, end - start)
print(end - start)
save(result_gumbel_90_days, file = "/Users/ruiliu/Desktop/research/results/group_2_2_result_gumbel_90_days.RData")

start = Sys.time()
result_gumbel_180_days = fit_gumbel_copula_time_varying(df = group_2_2, theta = rep(2, 5), period = 180, n_nodes = n_nodes)
end = Sys.time()
gumbel_times = c(gumbel_times, end - start)
print(end - start)
save(result_gumbel_180_days, file = "/Users/ruiliu/Desktop/research/results/group_2_2_result_gumbel_180_days.RData")

start = Sys.time()
result_gumbel_360_days = fit_gumbel_copula_time_varying(df = group_2_2, theta = rep(2, 5), period = 360, n_nodes = n_nodes)
end = Sys.time()
gumbel_times = c(gumbel_times, end - start)
print(end - start)
save(result_gumbel_360_days, file = "/Users/ruiliu/Desktop/research/results/group_2_2_result_gumbel_360_days.RData")

start = Sys.time()
result_gumbel_720_days = fit_gumbel_copula_time_varying(df = group_2_2, theta = rep(2, 5), period = 720, n_nodes = n_nodes)
end = Sys.time()
gumbel_times = c(gumbel_times, end - start)
print(end - start)
save(result_gumbel_720_days, file = "/Users/ruiliu/Desktop/research/results/group_2_2_result_gumbel_720_days.RData")

save(gumbel_times, file = "/Users/ruiliu/Desktop/research/results/group_2_2_gumbel_time_varying_times.RData")


gumbel_times = c()
n_nodes = 35

start = Sys.time()
result_gumbel_90_days = fit_gumbel_copula_time_varying(df = group_2_3, theta = rep(2, 8), period = 90, n_nodes = n_nodes)
end = Sys.time()
gumbel_times = c(gumbel_times, end - start)
print(end - start)
save(result_gumbel_90_days, file = "/Users/ruiliu/Desktop/research/results/group_2_3_result_gumbel_90_days.RData")

start = Sys.time()
result_gumbel_180_days = fit_gumbel_copula_time_varying(df = group_2_3, theta = rep(2, 8), period = 180, n_nodes = n_nodes)
end = Sys.time()
gumbel_times = c(gumbel_times, end - start)
print(end - start)
save(result_gumbel_180_days, file = "/Users/ruiliu/Desktop/research/results/group_2_3_result_gumbel_180_days.RData")

start = Sys.time()
result_gumbel_360_days = fit_gumbel_copula_time_varying(df = group_2_3, theta = rep(2, 8), period = 360, n_nodes = n_nodes)
end = Sys.time()
gumbel_times = c(gumbel_times, end - start)
print(end - start)
save(result_gumbel_360_days, file = "/Users/ruiliu/Desktop/research/results/group_2_3_result_gumbel_360_days.RData")

start = Sys.time()
result_gumbel_720_days = fit_gumbel_copula_time_varying(df = group_2_3, theta = rep(2, 8), period = 720, n_nodes = n_nodes)
end = Sys.time()
gumbel_times = c(gumbel_times, end - start)
print(end - start)
save(result_gumbel_720_days, file = "/Users/ruiliu/Desktop/research/results/group_2_3_result_gumbel_720_days.RData")

save(gumbel_times, file = "/Users/ruiliu/Desktop/research/results/group_2_3_gumbel_time_varying_times.RData")


rgumbel_times = c()
n_nodes = 35

start = Sys.time()
result_rgumbel_90_days = fit_rgumbel_copula_time_varying(df = group_1_1, theta = rep(2, 5), period = 90, n_nodes = n_nodes)
end = Sys.time()
rgumbel_times = c(rgumbel_times, end - start)
print(end - start)
save(result_rgumbel_90_days, file = "/Users/ruiliu/Desktop/research/results/group_1_1_result_rgumbel_90_days.RData")

start = Sys.time()
result_rgumbel_180_days = fit_rgumbel_copula_time_varying(df = group_1_1, theta = rep(2, 5), period = 180, n_nodes = n_nodes)
end = Sys.time()
rgumbel_times = c(rgumbel_times, end - start)
print(end - start)
save(result_rgumbel_180_days, file = "/Users/ruiliu/Desktop/research/results/group_1_1_result_rgumbel_180_days.RData")

start = Sys.time()
result_rgumbel_360_days = fit_rgumbel_copula_time_varying(df = group_1_1, theta = rep(2, 5), period = 360, n_nodes = n_nodes)
end = Sys.time()
rgumbel_times = c(rgumbel_times, end - start)
print(end - start)
save(result_rgumbel_360_days, file = "/Users/ruiliu/Desktop/research/results/group_1_1_result_rgumbel_360_days.RData")

start = Sys.time()
result_rgumbel_720_days = fit_rgumbel_copula_time_varying(df = unif_df, theta = rep(2, 5), period = 720, n_nodes = n_nodes)
end = Sys.time()
rgumbel_times = c(rgumbel_times, end - start)
print(end - start)
save(result_rgumbel_720_days, file = "/Users/ruiliu/Desktop/research/results/group_1_1_result_rgumbel_720_days.RData")

save(rgumbel_times, file = "/Users/ruiliu/Desktop/research/results/group_1_1_rgumbel_time_varying_times.RData")



rgumbel_times = c()
n_nodes = 35

start = Sys.time()
result_rgumbel_90_days = fit_rgumbel_copula_time_varying(df = group_1_2, theta = rep(2, 5), period = 90, n_nodes = n_nodes)
end = Sys.time()
rgumbel_times = c(rgumbel_times, end - start)
print(end - start)
save(result_rgumbel_90_days, file = "/Users/ruiliu/Desktop/research/results/group_1_2_result_rgumbel_90_days.RData")

start = Sys.time()
result_rgumbel_180_days = fit_rgumbel_copula_time_varying(df = group_1_2, theta = rep(2, 5), period = 180, n_nodes = n_nodes)
end = Sys.time()
rgumbel_times = c(rgumbel_times, end - start)
print(end - start)
save(result_rgumbel_180_days, file = "/Users/ruiliu/Desktop/research/results/group_1_2_result_rgumbel_180_days.RData")

start = Sys.time()
result_rgumbel_360_days = fit_rgumbel_copula_time_varying(df = group_1_2, theta = rep(2, 5), period = 360, n_nodes = n_nodes)
end = Sys.time()
rgumbel_times = c(rgumbel_times, end - start)
print(end - start)
save(result_rgumbel_360_days, file = "/Users/ruiliu/Desktop/research/results/group_1_2_result_rgumbel_360_days.RData")

start = Sys.time()
result_rgumbel_720_days = fit_rgumbel_copula_time_varying(df = group_1_2, theta = rep(2, 5), period = 720, n_nodes = n_nodes)
end = Sys.time()
rgumbel_times = c(rgumbel_times, end - start)
print(end - start)
save(result_rgumbel_720_days, file = "/Users/ruiliu/Desktop/research/results/group_1_2_result_rgumbel_720_days.RData")

save(rgumbel_times, file = "/Users/ruiliu/Desktop/research/results/group_1_2_rgumbel_time_varying_times.RData")


rgumbel_times = c()
n_nodes = 35

start = Sys.time()
result_rgumbel_90_days = fit_rgumbel_copula_time_varying(df = group_1_3, theta = rep(2, 10), period = 90, n_nodes = n_nodes)
end = Sys.time()
rgumbel_times = c(rgumbel_times, end - start)
print(end - start)
save(result_rgumbel_90_days, file = "/Users/ruiliu/Desktop/research/results/group_1_3_result_rgumbel_90_days.RData")

start = Sys.time()
result_rgumbel_180_days = fit_rgumbel_copula_time_varying(df = group_1_3, theta = rep(2, 10), period = 180, n_nodes = n_nodes)
end = Sys.time()
rgumbel_times = c(rgumbel_times, end - start)
print(end - start)
save(result_rgumbel_180_days, file = "/Users/ruiliu/Desktop/research/results/group_1_3_result_rgumbel_180_days.RData")

start = Sys.time()
result_rgumbel_360_days = fit_rgumbel_copula_time_varying(df = group_1_3, theta = rep(2, 10), period = 360, n_nodes = n_nodes)
end = Sys.time()
rgumbel_times = c(rgumbel_times, end - start)
print(end - start)
save(result_rgumbel_360_days, file = "/Users/ruiliu/Desktop/research/results/group_1_3_result_rgumbel_360_days.RData")

start = Sys.time()
result_rgumbel_720_days = fit_rgumbel_copula_time_varying(df = group_1_3, theta = rep(2, 10), period = 720, n_nodes = n_nodes)
end = Sys.time()
rgumbel_times = c(rgumbel_times, end - start)
print(end - start)
save(result_rgumbel_720_days, file = "/Users/ruiliu/Desktop/research/results/group_1_3_result_rgumbel_720_days.RData")

save(rgumbel_times, file = "/Users/ruiliu/Desktop/research/results/group_1_3_rgumbel_time_varying_times.RData")

rgumbel_times = c()
n_nodes = 35

start = Sys.time()
result_rgumbel_90_days = fit_rgumbel_copula_time_varying(df = group_2_1, theta = rep(2, 7), period = 90, n_nodes = n_nodes)
end = Sys.time()
rgumbel_times = c(rgumbel_times, end - start)
print(end - start)
save(result_rgumbel_90_days, file = "/Users/ruiliu/Desktop/research/results/group_2_1_result_rgumbel_90_days.RData")

start = Sys.time()
result_rgumbel_180_days = fit_rgumbel_copula_time_varying(df = group_2_1, theta = rep(2, 7), period = 180, n_nodes = n_nodes)
end = Sys.time()
rgumbel_times = c(rgumbel_times, end - start)
print(end - start)
save(result_rgumbel_180_days, file = "/Users/ruiliu/Desktop/research/results/group_2_1_result_rgumbel_180_days.RData")

start = Sys.time()
result_rgumbel_360_days = fit_rgumbel_copula_time_varying(df = group_2_1, theta = rep(2, 7), period = 360, n_nodes = n_nodes)
end = Sys.time()
rgumbel_times = c(rgumbel_times, end - start)
print(end - start)
save(result_rgumbel_360_days, file = "/Users/ruiliu/Desktop/research/results/group_2_1_result_rgumbel_360_days.RData")

start = Sys.time()
result_rgumbel_720_days = fit_rgumbel_copula_time_varying(df = group_2_1, theta = rep(2, 7), period = 720, n_nodes = n_nodes)
end = Sys.time()
rgumbel_times = c(rgumbel_times, end - start)
print(end - start)
save(result_rgumbel_720_days, file = "/Users/ruiliu/Desktop/research/results/group_2_1_result_rgumbel_720_days.RData")

save(rgumbel_times, file = "/Users/ruiliu/Desktop/research/results/group_2_1_rgumbel_time_varying_times.RData")


rgumbel_times = c()
n_nodes = 35

start = Sys.time()
result_rgumbel_90_days = fit_rgumbel_copula_time_varying(df = group_2_2, theta = rep(2, 5), period = 90, n_nodes = n_nodes)
end = Sys.time()
rgumbel_times = c(rgumbel_times, end - start)
print(end - start)
save(result_rgumbel_90_days, file = "/Users/ruiliu/Desktop/research/results/group_2_2_result_rgumbel_90_days.RData")

start = Sys.time()
result_rgumbel_180_days = fit_rgumbel_copula_time_varying(df = group_2_2, theta = rep(2, 5), period = 180, n_nodes = n_nodes)
end = Sys.time()
rgumbel_times = c(rgumbel_times, end - start)
print(end - start)
save(result_rgumbel_180_days, file = "/Users/ruiliu/Desktop/research/results/group_2_2_result_rgumbel_180_days.RData")

start = Sys.time()
result_rgumbel_360_days = fit_rgumbel_copula_time_varying(df = group_2_2, theta = rep(2, 5), period = 360, n_nodes = n_nodes)
end = Sys.time()
rgumbel_times = c(rgumbel_times, end - start)
print(end - start)
save(result_rgumbel_360_days, file = "/Users/ruiliu/Desktop/research/results/group_2_2_result_rgumbel_360_days.RData")

start = Sys.time()
result_rgumbel_720_days = fit_rgumbel_copula_time_varying(df = group_2_2, theta = rep(2, 5), period = 720, n_nodes = n_nodes)
end = Sys.time()
rgumbel_times = c(rgumbel_times, end - start)
print(end - start)
save(result_rgumbel_720_days, file = "/Users/ruiliu/Desktop/research/results/group_2_2_result_rgumbel_720_days.RData")

save(rgumbel_times, file = "/Users/ruiliu/Desktop/research/results/group_2_2_rgumbel_time_varying_times.RData")


rgumbel_times = c()
n_nodes = 35

start = Sys.time()
result_rgumbel_90_days = fit_rgumbel_copula_time_varying(df = group_2_3, theta = rep(2, 8), period = 90, n_nodes = n_nodes)
end = Sys.time()
rgumbel_times = c(rgumbel_times, end - start)
print(end - start)
save(result_rgumbel_90_days, file = "/Users/ruiliu/Desktop/research/results/group_2_3_result_rgumbel_90_days.RData")

start = Sys.time()
result_rgumbel_180_days = fit_rgumbel_copula_time_varying(df = group_2_3, theta = rep(2, 8), period = 180, n_nodes = n_nodes)
end = Sys.time()
rgumbel_times = c(rgumbel_times, end - start)
print(end - start)
save(result_rgumbel_180_days, file = "/Users/ruiliu/Desktop/research/results/group_2_3_result_rgumbel_180_days.RData")

start = Sys.time()
result_rgumbel_360_days = fit_rgumbel_copula_time_varying(df = group_2_3, theta = rep(2, 8), period = 360, n_nodes = n_nodes)
end = Sys.time()
rgumbel_times = c(rgumbel_times, end - start)
print(end - start)
save(result_rgumbel_360_days, file = "/Users/ruiliu/Desktop/research/results/group_2_3_result_rgumbel_360_days.RData")

start = Sys.time()
result_rgumbel_720_days = fit_rgumbel_copula_time_varying(df = group_2_3, theta = rep(2, 8), period = 720, n_nodes = n_nodes)
end = Sys.time()
rgumbel_times = c(rgumbel_times, end - start)
print(end - start)
save(result_rgumbel_720_days, file = "/Users/ruiliu/Desktop/research/results/group_2_3_result_rgumbel_720_days.RData")

save(rgumbel_times, file = "/Users/ruiliu/Desktop/research/results/group_2_3_rgumbel_time_varying_times.RData")
