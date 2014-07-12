#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector get1(NumericVector X, int n) {
  n = n - 1;
  return( X[n] );
}

// [[Rcpp::export]]
NumericVector get2(NumericVector X, int n) {
  n = n - 1;
  return( X(n) );
}
