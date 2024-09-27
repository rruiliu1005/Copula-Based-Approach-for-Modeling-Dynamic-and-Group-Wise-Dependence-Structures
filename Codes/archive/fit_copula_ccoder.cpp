#include <Rcpp.h>
#include <RcppNumerical.h>

using namespace Rcpp;
using namespace Numer;

// Function to compute the bivariate Gaussian copula
double bivariate_gaussian_copulas(double u1, double u2, double rho) {
  double x1 = R::qnorm(u1, 0, 1, true, false);
  double x2 = R::qnorm(u2, 0, 1, true, false);
  rho = std::min(std::max(rho, -1.0 + 1e-12), 1.0 - 1e-12);
  double num = rho*x1*x2 - 0.5*rho*rho*(x1*x1 + x2*x2)
  double den = 1 - rho * rho;
  return exp( num / den + 0.5 * (x1 * x1 + x2 * x2)) / sqrt(den);
}

// Function to compute the joint Gaussian copula
double gaussian_joint_copulas(const NumericVector& u, const NumericVector& rho, double v1) {
  int n = u.size();
  double result = 1.0;
  for (int i = 0; i < n; ++i) {
    result *= bivariate_gaussian_copulas(u[i], v1, rho[i]);
  }
  return result;
}

// Define the integrand for adaptIntegrate
class Integrand : public Func {
private:
  NumericVector u;
  NumericVector rho;
  
public:
  Integrand(const NumericVector& u_, const NumericVector& rho_) : u(u_), rho(rho_) {}
  
  double operator()(const double& v1) const {
    return gaussian_joint_copulas(u, rho, v1);
  }
};

// Function to compute the log likelihood
// [[Rcpp::export]]
double gaussian_log_likelihood(const NumericVector& rho, const NumericMatrix& u) {
  if (is_true(any(rho < -0.99)) || is_true(any(rho > 0.99))) return 9e100;
  
  double log_likelihood = 0;
  for (int i = 0; i < u.nrow(); ++i) {
    Integrand integrand(u(i, _), rho);
    double err_est;
    int err_code;
    double res = integrate(integrand, 0.0, 1.0, err_est, err_code);
    log_likelihood += log(res);
  }
  
  //Rcout << -log_likelihood << "\n";
  return -log_likelihood;
}

// Function to perform optimization using the nlm equivalent
// [[Rcpp::export]]
List optimize_gaussian_log_likelihood(const NumericVector& initial_rho, const NumericMatrix& u, int iterlim = 100, double steptol = 0.01) {
  Rcpp::Environment stats("package:stats");
  Rcpp::Function nlm = stats["nlm"];
  List res = nlm(Rcpp::_["f"] = Rcpp::InternalFunction(&gaussian_log_likelihood), 
                 Rcpp::_["p"] = initial_rho, 
                 Rcpp::_["u"] = u, 
                 Rcpp::_["iterlim"] = iterlim, 
                 Rcpp::_["steptol"] = steptol);
  return res;
}
