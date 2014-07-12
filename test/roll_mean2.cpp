// this solution seems to be slower...

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector roll_mean2( NumericVector X, int n ) {
  
  int len = X.size();
  int len_out = len - n + 1;
  
  NumericVector out = no_init( len_out );
  NumericVector tmp = no_init( n ); // store intermediate values here
  
  for( int i=0; i < len_out; i++ ) {
    // pop off the first element, push back the last one
    for( int k=0; k < n; k++ ) {
      tmp[k] = X[k+i];
    }
    out[i] = mean( tmp );
  }
  
  return( out );
  
}