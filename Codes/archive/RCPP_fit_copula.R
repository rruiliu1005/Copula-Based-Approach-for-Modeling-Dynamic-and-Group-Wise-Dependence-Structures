Sys.setenv(PKG_CPPFLAGS = "-I/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/RcppNumerical/include -I/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/RcppEigen/include -I/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/RcppParallel/include")
library(Rcpp)
setwd('/Users/ruiliu/Desktop/research')
sourceCpp("./codes/fit_copula_ccoder.cpp")
unif_df = read.csv('./data/unif_df.csv')



# Example usage in R
initial_rho <- rep(0.5, (ncol(unif_df) - 1))
#initial_rho <- rep(0.5, 20)
start <- Sys.time()
gaussian_result <- optimize_gaussian_log_likelihood(initial_rho, as.matrix(unif_df[, 1:(ncol(unif_df) - 1)]), iterlim = 1000, steptol = 0.0001)
#gaussian_result <- optimize_gaussian_log_likelihood(initial_rho, as.matrix(unif_df[, 1:20]), iterlim = 100, steptol = 0.005)
end <- Sys.time()
gaussian_1f_time <- end - start
print(gaussian_1f_time)
#print(gaussian_result)

library(FactorCopula)



start <- Sys.time()
gaussian_1f <- mle1factor(continuous =unif_df[, 1:(ncol(unif_df) - 1)], copF1 = rep('bvn', (ncol(unif_df)-1)), gl = gauss.quad.prob(25))
end <- Sys.time()
gaussian_1f_time = end - start
print(gaussian_1f_time)