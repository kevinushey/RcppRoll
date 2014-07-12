#include <Rcpp.h>
using namespace Rcpp;

typedef NumericVector::iterator iter;
typedef NumericVector NV;

inline
double test( NV& x, NV& weights, const int n, const int N, const int ind) {
  double out_ = 0;
  for( int i=0; i < n; i++ ) {
    out_ += weights[i] * x[i+ind];
  }
  return out_;
}

// [[Rcpp::export]]
NumericVector test_numeric( NV x, int n, NV weights ) {
  
  int len = x.size();
  int len_out = len - n + 1;
  const int N = sum( sign( weights*weights ) );
  
  NV out = no_init( len_out );
  
  for( int ind=0; ind < len_out; ind++ ) {
    out[ind] = test( x, weights, n, N, ind );
  }
  
  return out;
  
}
