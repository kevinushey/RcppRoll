#include <Rcpp.h>
using namespace Rcpp;

inline
double zEgP9Bz1uFfOW9jIj21JY(NumericVector x, const NumericVector weights) {
  double out_ = 0;
  for( int i=0; i < x.size(); i++ ) {
    out_ += weights[i] * x[i];
  }
  return out_;
}

// [[Rcpp::export]]
NumericVector zEgP9Bz1uFfOW9jIj21JY_numeric( NumericVector x, int n, const NumericVector weights, const int by ) {
  
  int len = x.size();
  int len_out = len - n - by + 2;
  
  NumericVector out = no_init( len_out );
  
  NumericVector tmp = no_init( n );
  for( int i=0; i < len_out; i++ ) {
    for( int j=0; j < n; j++ ) {
      tmp[j] = x[i+j*by];
    }
    out[i] = zEgP9Bz1uFfOW9jIj21JY( tmp, weights );
  }
  
  return out;
  
}

// [[Rcpp::export]]
NumericMatrix zEgP9Bz1uFfOW9jIj21JY_matrix( NumericMatrix A, int n, bool by_column, const NumericVector weights, const int by ) {
  
  int nRow = A.nrow();
  int nCol = A.ncol();
  if( by_column ) {
    
    NumericMatrix B( nRow - n + 1, nCol );
    
    for( int j=0; j < nCol; j++ ) {
      
      NumericVector tmp = A(_, j);
      for( int i=0; i < nRow - n + 1; i++ ) {
        B(i, j) = zEgP9Bz1uFfOW9jIj21JY( tmp[ seq(i, i+n-1) ], weights, by );
      }
    }
    
    return B;
    
  } else {
    
    NumericMatrix B( nRow, nCol - n + 1 );
    
    for( int i=0; i < nRow; i++ ) {
      
      NumericVector tmp = A(i, _);
      for( int j=0; j < nCol - n + 1; j++ ) {
        B(i, j) = zEgP9Bz1uFfOW9jIj21JY( tmp[ seq(j, j+n-1) ], weights, by );
      }
    }
    
    return B;
    
  }
  
}
