#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double roll_ex(const std::vector<double>& x) {
  double y = 0;
  for( int i=0; i < x.size(); i++ ) {
    y += (x[i] * x[i]);
  }
  return y;
}