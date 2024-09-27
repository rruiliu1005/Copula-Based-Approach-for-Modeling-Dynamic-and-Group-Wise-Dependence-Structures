library(stats)
library(pracma)
library(FactorCopula)
library(cubature)
library(copula)
library(matrixStats)
setwd('/Users/ruiliu/Desktop/research')
df = read.csv('./data/unif_df.csv')
columns_to_keep = c('BTCUSDT', 'ETHUSDT', 'BNBUSDT', 'XRPUSDT', 'ADAUSDT', 'DOGEUSDT', 'SOLUSDT', 'XLMUSDT', 'MATICUSDT', 'TRXUSDT', 
                    'LTCUSDT', 'DOTUSDT', 'AVAXUSDT', 'NEARUSDT', 'FILUSDT', 'ATOMUSDT', 'FTMUSDT', 'LINKUSDT', 'UNIUSDT', 'ETCUSDT')
unif_df = df[, columns_to_keep]
head(unif_df)

#this function returns a vector of derivatives for a single observation
clayton_derivative_single_obs <- function(u1, nl_u2, theta, wl, d, nl){
  u1 = as.numeric(u1)
  A = 1+theta
  # a = 1+theta
  B1 = matrix(u1^(-theta), nrow = nl, ncol = d, byrow = TRUE) + outer(nl_u2,-theta, "^") - 1
  # u2 = nl_u2[5]
  #b1 = u1^(-theta)+u2^(-theta) - 1
  B = sweep(B1, 2, -2-1/theta, "^")
  #b = b1^(-2-1/theta)
  C = sweep(outer(nl_u2, u1, "*"), 2, -theta-1, "^")
  #c = (u1*u2)^(-theta-1)
  deriv_B = B*(
    sweep(log(B1), MARGIN = 2, STATS = 1/theta^2, FUN = "*") + sweep( 
    (-matrix(u1^(-theta)* log(u1), nrow = nl, ncol = d, byrow = TRUE) - outer(nl_u2, -theta, "^")* log(nl_u2))/B1
    , MARGIN = 2, STATS = (-2-1/theta), FUN = "*")
    )
  #deriv_b = b*(1/theta^2*log(b1)+(-2-1/theta)*(-u1^(-theta)*log(u1)-u2^(-theta)*log(u2))/b1)
  deriv_C = -C*log(outer(nl_u2, u1, "*"))
  deriv_c = -c*log(u1*u2)
  prelim = A*B*C
  prelim_likelihood = apply(prelim, MARGIN = 1, FUN = prod)
  likelihood = sum(prelim_likelihood*wl)
  
  deriv_copula = B*C + deriv_B*C + A*B*deriv_C
  #d_c = b*c+deriv_b*c+a*b*deriv_c
  #repeat the full product of the likelihood d times so we divide by each d likelihood later
  #each row corresponds to the quad points
  total_product_matrix = matrix(rep(prelim_log_likelihood, each = d), nrow = nl, byrow = TRUE)
  
  deriv_multiplier = total_product_matrix/prelim
  deriv_log_likelihood_quad = deriv_multiplier/likelihood
  deriv_log_likelihood = sweep(deriv_log_likelihood_quad, 1, wl, "*")
  deriv_log_likelihood = apply(deriv_log_likelihood_quad, MARGIN = 2, FUN = sum)
  
  return(list(log_likelihood = log(likelihood),  derivatives = deriv_log_likelihood))
}

clayton_derivative <- function(unif_df, theta, nl_u2, wl, d, nl){
  out = t(apply(unif_df, 1, clayton_derivative_single_obs, nl_u2 = nl_u2, theta = theta, wl = wl, d = d, nl = nl))
  log_likelihood <- sum(sapply(out, function(x) x$log_likelihood))
  derivatives <- rowSums(sapply(out, function(x) x$derivatives))
  return(list(log_likelihood = log_likelihood, derivatives = derivatives))
}

neg_log_clayton_likelihood <- function(unif_df, theta, nl_u2, wl, d, nl){
  #print("theta: ")
  #print(theta)
  if (min(theta) < 1){
    log_likelihood = 9e5
    attr(log_likelihood, "gradient") = rep(1e6*sign(theta), length(theta))
    return(log_likelihood)
  }
  else{
    result = clayton_derivative(unif_df, theta, nl_u2, wl, d, nl)
    log_likelihood = -1 * result$log_likelihood
    gradient = result$derivatives
    attr(log_likelihood, "gradient") = -1 * gradient
    print("gradient: ")
    print(attr(log_likelihood, "gradient"))
    return(log_likelihood)
    
  }
}

fit_clayton_copula <- function(unif_df, theta, n_nodes = 25){
  gl <- gaussLegendre(n_nodes, 0, 1)
  nl_u2 = gl$x
  wl <- gl$w
  d = ncol(unif_df)
  result_test = nlm(f = neg_log_clayton_likelihood, p = theta, unif_df = unif_df, 
                    nl_u2 = nl_u2, wl = wl, nl = n_nodes, d = d, gradtol = 1e-5, steptol = 1e-6,
                    hessian = TRUE, check.analyticals = TRUE)
  return(result_test)
}

d = 20
theta = rep(3, d)
fit_clayton_copula(unif_df[1:50, 1:d], theta)



#check derivative calculation is correct 
theta1 = rep(3,d)
theta2 = theta1
theta2[3] = 3.001
tmp = clayton_derivative_single_obs(unif_df[1,], nl_u2, theta1, wl, d, nl)
(tmp$log_likelihood - clayton_derivative_single_obs(unif_df[1,], nl_u2, theta2, wl, d, nl)$log_likelihood)/0.001
tmp$derivatives
