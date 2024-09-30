
#' Akaike information criterion 
#'
#' This function computes Akaike information criterion
#'
#' @param log_likelihood log likelihood function value 
#' @param num_parameters the number of parameters estimated 
#' @return the AIC as a number 
#' @export
computeAIC <- function(log_likelihood, num_parameters) {
  aic <- 2 * num_parameters - 2 * log_likelihood
  return(aic)
}
#' Bayesian information criterion
#'
#' This function computes Bayesian information criterion
#'
#' @param log_likelihood log likelihood function value 
#' @param num_parameters the number of parameters estimated 
#' @param num_observations the number of observations used to estimate the parameters 
#' @return the BIC as a number 
#' @export
computeBIC <- function(log_likelihood, num_parameters, num_observations) {
  bic <- num_parameters * log(num_observations) - 2 * log_likelihood
  return(bic)
}