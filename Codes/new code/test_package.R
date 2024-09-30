# devtools::document()
# devtools::build()
# devtools::install()

library(DDFactorCopula)
unif_df = read.csv('/Users/ruiliu/Desktop/research/data/unif_df.csv')

################################################
#test base model 
test_results = fitOneFactor(df = unif_df[,1:20],copula = "Gaussian", 
                          list(n_nodes = 35))

test_results = fitOneFactor(df = unif_df[,1:20],copula = "Clayton", 
                            list(n_nodes = 35, init_val = rep(2, 20)))

test_results = fitOneFactor(df = unif_df[,1:20],copula = "Gumbel", 
                            list(n_nodes = 35, init_val = rep(3, 20)))

test_results = fitOneFactor(df = unif_df[,1:20],copula = "rGumbel", 
                            list(n_nodes = 35, init_val = rep(1, 20)))
################################################
#test dynamic model 
test_results = fitOneFactorDynamic(df = unif_df[1:200,1:20], copula = 'Gaussian', 
                                   list(n_nodes = 35, init_val = rep(0.5, 20), period = 90))

test_results = fitOneFactorDynamic(df = unif_df[1:200,1:20], copula = 'Clayton', 
                                   list(n_nodes = 25, init_val = rep(2, 20), period = 90))

test_results = fitOneFactorDynamic(df = unif_df[1:200,1:20], copula = 'Gumbel', 
                                   list(period = 90))

test_results = fitOneFactorDynamic(df = unif_df[1:200,1:20], copula = 'rGumbel', 
                                   list(n_nodes = 25))

################################################
#test tail weighted measure of dependence function 
test_results = powerTailDependence(unif_df[,1:20], d = 6, p = 0.5)

################################################
#test AIC BIC functions 
test_results = computeAIC(12000, 3)
test_results = computeBIC(12000, 3, 1000)
################################################
#test generate function
test_results = simulateOneFactor(1000, "Gaussian", 8, c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8))
test_results = simulateOneFactor(1000, "Clayton", 8, c(1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8))
test_results = simulateOneFactor(1000, "Gumbel", 8, c(1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8))
test_results = simulateOneFactor(1000, "rGumbel", 8, c(1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8))
  


