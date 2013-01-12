// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix test( NumericMatrix x, int n ) {
  arma::mat X = as<arma::mat>(x);
  return wrap<NumericMatrix>( X.col(0) );
}