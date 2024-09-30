#' Simulate from a one factor copula 
#'
#' This function simulates from a one factor copula model 
#'
#' @param n number of observations to simulate 
#' @param copula the copula to simulate from, can be one of Gaussian, Clayton, Gumbel and rGumbel
#' @param d number of variables 
#' @param theta the copula parameters as a vector 
#' @importFrom FactorCopula r1factor
#' @return A ncol(df) x ncol(df) matrix containing the tail weighted measure of dependence between the variables
#' @export
simulateOneFactor = function(n, copula, d, theta){
  if (copula == "Gaussian"){
    return(r1factor(n = n, d1 = d, theta = theta, copF1 = rep('bvn', d)))
  }
  else if (copula == "Clayton"){
    params = Map(function(x,y) mat <- matrix(c(x, y), nrow = 1, ncol = 2), theta, rep(1, d))
    return(r1factor(n = n, d1 = d, theta = params, copF1 = rep('bb1', d)))
  }
  else if (copula == "Gumbel"){
    return(r1factor(n = n,  d1 = d, theta = theta, copF1 = rep('gum', d)))
  }
  else if (copula == "rGumbel"){
    return(r1factor(n = n,  d1 = d, theta = theta, copF1 = rep('rgum', d)))
  }
}