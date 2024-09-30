library(stats)
library(pracma)
library(FactorCopula)
library(cubature)
library(copula)
library(matrixStats)

gaussian_derivative_single_obs = function(u1, rho, nl_x2, wl){
  rho_squared = rho^2
  one_minus_rho_squared = 1 - rho_squared
  u1 = as.numeric(u1)
  x1 = qnorm(u1, mean = 0, sd = 1)
  x2 = matrix(rep(nl_x2, 20), ncol = 35, nrow = 20, byrow = TRUE)

  numerator = 2 * rho * x1 * x2 - rho_squared * (x1^2 + x2^2)
  denominator = 2 * one_minus_rho_squared
  exponent = numerator / denominator
  out = exp(exponent) / sqrt(one_minus_rho_squared)
  prod_vec = apply(out, 2, prod)
  ##########calcualte numerator
  # we have a d by |nl| matrix (columns are the derivative values for each nl)
  x2 = matrix(rep(nl_x2, 20), ncol = 35, nrow = 20, byrow = TRUE)
  deriv_mul_vec = (rho * one_minus_rho_squared + (1 + rho_squared) * x1 * x2 - rho * (x1^2 + x2^2)) / (one_minus_rho_squared^2)
  num = deriv_mul_vec%*%diag(prod_vec) %*% diag(wl)
  num = rowSums(num)
  ##########calcualte denominator
  denom = sum(prod_vec * wl)
  result = num/denom
  return(list(log_likelihood = log(denom),  derivatives = result))
}

#this function returns a vector of derivatives for a all observations, and the log likelihood for all observations
gaussian_derivative = function(unif_df, rho, nl_x2, wl){
  out = t(apply(unif_df, 1, gaussian_derivative_single_obs, rho = rho, nl_x2 = nl_x2, wl = wl))
  log_likelihood = sum(sapply(out, function(x) x$log_likelihood))
  derivatives = rowSums(sapply(out, function(x) x$derivatives))
  return(list(log_likelihood = log_likelihood, derivatives = derivatives))
}

neg_log_gaussian_likelihood = function(unif_df, rho, nl_x2, wl){
  # print("rho: ")
  # print(rho)
  if ((max(rho) > 0.9999999) | (min(rho) < -0.9999999) | rho[1]<0){
    log_likelihood = 9e5
    attr(log_likelihood, "gradient") = 1e6*sign(rho)
    return(log_likelihood)
  }
  result = gaussian_derivative(unif_df, rho, nl_x2, wl)
  log_likelihood = -1 * result$log_likelihood
  gradient = result$derivatives
  attr(log_likelihood, "gradient") = -1 * gradient
  return(log_likelihood)
}

fit_gaussian_copula = function(unif_df, rho, n_nodes){
  gl = gaussLegendre(n_nodes, 0, 1)
  nl_x2 = qnorm(gl$x)
  wl = gl$w
  d = ncol(unif_df)
  result_test = nlm(f = neg_log_gaussian_likelihood, p = rho, unif_df = unif_df, nl_x2 = nl_x2, wl = wl, gradtol = 1e-5, steptol = 1e-10, iterlim = 1000, hessian = FALSE, check.analyticals = FALSE)
  return(result_test)
}


clayton_single_obs_pre = function(u1, u2, theta, d) {
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


clayton_single_obs = function(u1, theta, d, n_nodes, nl_u2, wl){
  tmp = mapply(clayton_single_obs_pre, u2 = nl_u2, MoreArgs = list(u1 = u1, theta = theta, d))
  numerator_matrix = do.call(rbind, tmp[1,])
  numerator = apply(numerator_matrix * wl, 2, sum)
  denominator = sum(unlist(tmp[2,])*wl)

  deriv_log_likelihood = numerator / denominator
  log_likelihood = log(denominator)
  # attr(log_likelihood, "gradient") = deriv_log_likelihood
  # return(log_likelihood)
  return(list(log_likelihood = log_likelihood, deriv_log_likelihood = deriv_log_likelihood))
}

neg_clayton_loglikelihood = function(unif_df, theta, d, nl_u2, wl){
  if (min(theta) < 1){
    log_likelihood = 9e5
    attr(log_likelihood, "gradient") = 1e6*sign(theta)
    return(log_likelihood)
  }
  out = t(apply(unif_df, 1, clayton_single_obs, theta = theta, d = d,nl_u2 = nl_u2, wl = wl))
  log_likelihood = sum(sapply(out, function(x) x$log_likelihood))
  derivatives = rowSums(sapply(out, function(x) x$deriv_log_likelihood))

  neg_log_likelihood = -1 * log_likelihood
  attr(neg_log_likelihood, "gradient") = -1 * derivatives
  return(neg_log_likelihood)
}

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



neg_gumbel_loglikelihood = function(unif_df, theta, d, n_nodes){
  if (min(theta) < 1){
    log_likelihood = 9e5
    attr(log_likelihood, "gradient") = 1e6*sign(theta)
    return(log_likelihood)
  }
  out = t(apply(unif_df, 1, gumbel_single_obs, theta = theta, d = d, n_nodes = n_nodes))
  log_likelihood = sum(sapply(out, function(x) x$log_likelihood))
  derivatives = rowSums(sapply(out, function(x) x$deriv_log_likelihood))

  neg_log_likelihood = -1 * log_likelihood
  attr(neg_log_likelihood, "gradient") = -1 * derivatives
  return(neg_log_likelihood)
}

#' Fit a One-Factor Copula Model
#'
#' This function fits a one-factor copula model to the given data frame of uniform variables.
#' Supported copulas include 'Gaussian', 'Clayton', 'Gumbel', and 'rGumbel'.
#'
#' @param df A data frame containing uniform variables with \code{nrow(df)}
#' observations and \code{ncol(df)} variables.
#' @param copula A character string specifying the copula to be fitted.
#' Supported values are \code{'Gaussian'}, \code{'Clayton'}, \code{'Gumbel'}, and \code{'rGumbel'}.
#' @param params A list of parameters for the copula model.
#' Default is \code{list(n_nodes = 35, init_val = rep(0.5, ncol(df)))} for Gaussian copula,
#' \code{list(n_nodes = 25, init_val = rep(2, ncol(df)))} for Clayton, Gumbel and reflected Gumbel copulas
#' .
#' @return A list containing:
#'   \describe{
#'     \item{\code{minimum}}{The estimated minimum of the negative log-likelihood.}
#'     \item{\code{estimate}}{The optimal parameter values.}
#'     \item{\code{gradient}}{The gradient at the estimated minimum.}
#'   }
#' @export
fitOneFactor <- function(df, copula, params = list()) {
  d = ncol(df)
  if (copula == 'Gaussian') {
    default_params <- list(n_nodes = 35, init_val = rep(0.5, d))
    params <- modifyList(default_params, params)
    n_nodes <- params$n_nodes
    init_val <- params$init_val
    if (max(abs(init_val)) > 1){
      return("invalid copula initial parameters")
    }
    return(fit_gaussian_copula(df, init_val, n_nodes))
  }
  else if (copula == 'Clayton'){
    default_params <- list(n_nodes = 35, init_val = rep(2, d))
    params <- modifyList(default_params, params)
    n_nodes <- params$n_nodes
    init_val <- params$init_val
    if (min(init_val) < 0){
      return("invalid copula initial parameters")
    }
    gl = gaussLegendre(n = n_nodes, a = 0, b = 1)
    nl_u2 = gl$x
    wl = gl$w
    return(nlm(f = neg_clayton_loglikelihood, p = init_val, unif_df = df,
               nl_u2 = nl_u2, wl = wl, d = d, gradtol = 1e-5, hessian = FALSE,
               check.analyticals = FALSE))
  }
  else if (copula == 'Gumbel'){
    default_params <- list(n_nodes = 35, init_val = rep(2, d))
    params <- modifyList(default_params, params)
    n_nodes <- params$n_nodes
    init_val <- params$init_val
    if (min(init_val) < 1){
      return("invalid copula initial parameters")
    }
    gl = gaussLegendre(n = n_nodes, a = 0, b = 1)
    nl_u2 = gl$x
    wl = gl$w
    return(nlm(f = neg_gumbel_loglikelihood, p = init_val, unif_df = df,
               n_nodes = n_nodes, d = d, gradtol = 1e-5, hessian = FALSE,
               check.analyticals = FALSE))
  }
  else if (copula == 'rGumbel'){
    default_params <- list(n_nodes = 35, init_val = rep(2, d))
    params <- modifyList(default_params, params)
    n_nodes <- params$n_nodes
    init_val <- params$init_val
    if (min(init_val) < 1){
      return("invalid copula initial parameters")
    }
    gl = gaussLegendre(n = n_nodes, a = 0, b = 1)
    nl_u2 = gl$x
    wl = gl$w
    return(nlm(f = neg_gumbel_loglikelihood, p = init_val, unif_df = 1-df,
               n_nodes = n_nodes, d = d, gradtol = 1e-5, hessian = FALSE,
               check.analyticals = FALSE))
  }
}



