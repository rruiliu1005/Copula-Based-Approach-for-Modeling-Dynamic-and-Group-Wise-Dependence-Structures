#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double add_numbers(double x, double y) {
  return x + y;
}

/*** R
# Test the function in R
print(add_numbers(1, 2))
***/
