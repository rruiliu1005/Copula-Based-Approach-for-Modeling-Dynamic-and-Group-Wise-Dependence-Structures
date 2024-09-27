

library(stats)
library(pracma)
setwd('/Users/ruiliu/Desktop/research')
unif_df = read.csv('./data/unif_df.csv')
#unif_df = unif_df[, 2:length(unif_df)]
head(unif_df)
aa = read.csv('./data/unif_df_old.csv')
#install.packages('FactorCopula')
head(aa)

load('./models/Gaussian_1_factor.Rdata')
library(dplyr)
library(stats)
library(pracma)
library(cubature)

# Define the bivariate Gaussian copulas function
bivariate_gaussian_copulas <- function(u1, u2, rho) {
  x1 <- qnorm(u1, mean = 0, sd = 1)
  x2 <- qnorm(u2, mean = 0, sd = 1)
  #rho <- min(max(rho, -1 + 1e-12), 1 - 1e-12)
  out <- exp(-1/2 * (x1^2 + x2^2 - 2 * rho * x1 * x2) / (1 - rho^2) + 1/2 * (x1^2 + x2^2))/sqrt(1-rho^2)
  return(out)
}

# Define the joint copulas function
gaussian_joint_copulas <- function(u, rho, v1, n) {
  #function inside the integral
  result <- prod(sapply(1:n, function(i) bivariate_gaussian_copulas(u[i], v1, rho[i])))
  return(result)
}

# Define the log likelihood function
gaussian_log_likelihood <- function(rho, u) {
  integrand <- function(v1, u) gaussian_joint_copulas(u, rho, v1, n)
  if ((min(rho) < -0.99) || (max(rho) > 0.99)) return(9e100)
  integral <- sapply(1:nrow(u), function(i) adaptIntegrate(integrand, lowerLimit = 0, upperLimit = 1, u = u[i, ])$integral)
  log_likelihood <- sum(log(integral))
  #  cat(-log_likelihood, "\n")
  #  cat(rho, "\n")
  print(-log_likelihood)
  return(-log_likelihood)
}

# Initial rho values
initial_rho <- rep(0.5, (ncol(unif_df)-1))

# Minimize the log likelihood
start <- Sys.time()
gaussian_result <- nlm(f = gaussian_log_likelihood, p = initial_rho, u = as.matrix(unif_df[,1:(ncol(unif_df)-1)]), n = ncol(unif_df[,1:(ncol(unif_df)-1)]), iterlim = 100, steptol = 0.01)
#gaussian_result <- nlm(f = gaussian_log_likelihood, p = initial_rho, u = as.matrix(unif_df), iterlim = 100, steptol = 0.01, stepmax =50)
end <- Sys.time()
gaussian_1f_time = end - start
print(gaussian_1f_time)



# Define the bivariate Frank copulas function
bivariate_frank_copula <- function(u1, u2, theta) {
  out <- theta*(1-exp(-theta))*exp(-theta*(u1+u2))*((1-exp(-theta))-(1-exp(-theta*u1))*(1-exp(-theta*u2)))^(-2)
  return(out)
}

# Define the joint copulas function
frank_joint_copulas <- function(u, theta, v1) {
  #function inside the integral
  result <- prod(sapply(1:9, function(i) bivariate_frank_copula(u[i], v1, theta[i])))
  return(result)
}

# Define the log likelihood function
frank_log_likelihood <- function(theta, u) {
  integrand <- function(v1, u) frank_joint_copulas(u, theta, v1)
  if (sum(theta == 0) > 0) return(9e100)
  
  integral <- sapply(1:nrow(u), function(i) adaptIntegrate(integrand, lowerLimit = 0, upperLimit = 1, u = u[i, ])$integral)
  log_likelihood <- sum(log(integral))
  #  cat(-log_likelihood, "\n")
  #  cat(theta, "\n")
  return(-log_likelihood)
}

# Initial theta values
initial_theta <- rep(3, 9)

# Minimize the log likelihood
start <- Sys.time()
result <- nlm(f = frank_log_likelihood, p = initial_theta, u = as.matrix(unif_df), iterlim = 50, steptol = 0.001)
end <- Sys.time()
print(end - start)

bivariate_clayton_copula <- function(u1, u2, theta) {
  out <- (1+theta)*(u1*u2)^(-1-theta)*(u1^(-theta)+u2^(-theta)-1)^(-1/theta-2)
  return(out)
}

# Define the joint copulas function
clayton_joint_copulas <- function(u, theta, v1) {
  #function inside the integral
  result <- prod(sapply(1:9, function(i) bivariate_clayton_copula(u[i], v1, theta[i])))
  return(result)
}

# Define the log likelihood function
clayton_log_likelihood <- function(theta, u) {
  integrand <- function(v1, u) clayton_joint_copulas(u, theta, v1)
  if (sum(theta < 0) > 0) return(9e100)
  
  integral <- sapply(1:nrow(u), function(i) adaptIntegrate(integrand, lowerLimit = 0, upperLimit = 1, u = u[i, ])$integral)
  log_likelihood <- sum(log(integral))
  #  cat(-log_likelihood, "\n")
  #  cat(theta, "\n")
  return(-log_likelihood)
}

# Initial theta values
initial_theta <- rep(3, 9)

# Minimize the log likelihood
start <- Sys.time()
result <- nlm(f = clayton_log_likelihood, p = initial_theta, u = as.matrix(unif_df), iterlim = 50, steptol = 0.001)
end <- Sys.time()
print(end - start)


bivariate_reflective_gumbel_copula <- function(u1, u2, theta) {
  tmp1 = exp(-((-log(u1))^theta+(-log(u2))^theta)^(1/theta))
  tmp2 = (u1*u2)^(-1)*((-log(u1))^theta + (-log(u2))^theta)^(-2+2/theta)*(log(u1)*log(u2))^(theta-1)
  tmp3 = (1+(theta-1)*((-log(u1))^theta + (-log(u2))^theta)^(-1/theta))
  out = tmp1*tmp2*tmp3
  return(out)
}

# Define the joint copulas function
reflective_gumbel_joint_copulas <- function(u, theta, v1) {
  #function inside the integral
  result <- prod(sapply(1:9, function(i) bivariate_reflective_gumbel_copula(u[i], v1, theta[i])))
  return(result)
}

# Define the log likelihood function
reflective_gumbel_log_likelihood <- function(theta, u) {
  integrand <- function(v1, u) reflective_gumbel_joint_copulas(u, theta, v1)
  if (sum(theta < 1) > 0) return(9e100)
  
  integral <- sapply(1:nrow(u), function(i) adaptIntegrate(integrand, lowerLimit = 0, upperLimit = 1, u = u[i, ])$integral)
  log_likelihood <- sum(log(integral))
  #  cat(-log_likelihood, "\n")
  #  cat(theta, "\n")
  return(-log_likelihood)
}

# Initial theta values
initial_theta <- rep(3, 9)

# Minimize the log likelihood
start <- Sys.time()
inverse_unif_df = 1 - unif_df
result <- nlm(f = reflective_gumbel_log_likelihood, p = initial_theta, u = as.matrix(inverse_unif_df), iterlim = 50, steptol = 0.001)
end <- Sys.time()
print(end - start)
head(unif_df)



#########################################MH Code 

library(cubature)
library(tmvtnorm)
bivariate_gaussian_copulas <- function(u1, u2, rho) {
  x1 <- qnorm(u1, mean = 0, sd = 1)
  x2 <- qnorm(u2, mean = 0, sd = 1)
  out <- exp(-1/2 * (x1^2 + x2^2 - 2 * rho * x1 * x2) / (1 - rho^2) + 1/2 * (x1^2 + x2^2)) / sqrt(1 - rho^2)
  return(out)
}

gaussian_joint_copulas <- function(u, rho, v, d) {
  # Function inside the integral
  result <- prod(sapply(1:d, function(i) bivariate_gaussian_copulas(u[i], v[i], rho[i])))
  return(result)
}

# Define the joint copulas function

gaussian_joint_copulas <- function(u, rho, v1) {
  # Function inside the integral
  d <- length(rho)
  result <- prod(sapply(1:d, function(i) bivariate_gaussian_copulas(u[i], v1, rho[i])))
  return(result)
}

# Define the log likelihood function
gaussian_log_likelihood <- function(rho, u) {
  if ((min(rho) < -0.99) || (max(rho) > 0.99)) return(9e100)
  integrand <- function(v1, u, rho){gaussian_joint_copulas(u, rho, v1)}
  integral <- adaptIntegrate(integrand, lowerLimit = 0, upperLimit = 1, u = u, rho = rho)$integral
  #log_likelihood <- sum(log(integral))
  #  cat(-log_likelihood, "\n")
  #  cat(rho, "\n")
  return(-integral)
}

gaussian_likelihood <- function(rho, u){
  return(exp(gaussian_log_likelihood(rho, u)))
}

# Metropolis-Hastings algorithm for multivariate distribution
metropolis_hastings_mv <- function(target_pdf, initial_value, n_samples, proposal_sd, rho_list) {
  d <- length(initial_value)
  samples <- matrix(NA, nrow = n_samples*2, ncol = d)
  samples[1, ] <- initial_value
  
  for (i in 2:(2*n_samples)) {
    current_value <- samples[i - 1, ]
    #proposed_value = runif(d, min = 0, max = 1)
    proposed_value <- rtmvnorm(n = 1, mean = current_value, sigma = (proposal_sd * diag(d)), lower = rep(0, d), upper = rep(1, d))  # Proposal step
    proposal_numerator = dtmvnorm(x = current_value, mean = proposed_value, sigma = proposal_sd)
    proposal_denominator = dtmvnorm(x = proposed_value, mean = current_value, sigma = proposal_sd)
    
    target_numerator = gaussian_likelihood(rho = rho_list, u = proposed_value)
    target_denominator = gaussian_likelihood(rho = rho_list, u = current_value )
    
    acceptance_ratio <- (proposal_numerator*target_numerator)/(proposal_denominator*target_denominator)
    acceptance_probability <- min(1, acceptance_ratio)
    u <- runif(1)
    if (u < acceptance_probability) {
      samples[i, ] <- proposed_value
    } else {
      samples[i, ] <- current_value
    }
  }
  
  return(samples[n_samples:(2*n_samples), ])
}

library(MASS)

initial_value <- rep(0.5, (ncol(unif_df) - 1))
n_samples <- nrow(unif_df)
proposal_sd <- apply(unif_df[,1:(ncol(unif_df)-1)], MARGIN = 2, FUN = sd)  # Standard deviation for the proposal distribution
rho_list = unlist(gaussian_1f$cpar$f1[1:(ncol(unif_df) - 1)])
samples <- metropolis_hastings_mv(gaussian_likelihood, initial_value, n_samples, proposal_sd, rho_list = rho_list)