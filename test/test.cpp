#include <Rcpp.h>
using namespace Rcpp;

typedef NumericVector::iterator iter;
typedef NumericVector NV;
typedef NumericMatrix NM;

// [[Rcpp::export]]
NumericVector roll_mean_numeric_vector( NumericVector x, int n, NumericVector weights ) {
  
  int len = x.size();
  int len_out = len - n + 1;
  int N = sum( sign( weights*weights ) );
  
  NV out = no_init( len_out );
  
  for( int i=0; i < len_out; i++ ) {
    
    out[i] = mean(x[ seq(i, i+n-1) ] );
  }
  
  return out;
  
}