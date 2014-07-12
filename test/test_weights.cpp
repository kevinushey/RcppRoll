#include <Rcpp.h>
using namespace Rcpp;

inline
double z0gmOBXiFbdNSicImCIt1(NumericVector x, NumericVector weights) {
  double out_ = 0;
  for( int i=0; i < x.size(); i++ ) {
    out_ += weights[i] * x[i];
  }
  return out_;
}

// [[Rcpp::export]]
NumericVector z0gmOBXiFbdNSicImCIt1_numeric( NumericVector x, int n, NumericVector weights ) {
  
  int len = x.size();
  int len_out = len - n + 1;
  
  NumericVector out = no_init( len_out );
  
  for( int i=0; i < len_out; i++ ) {
    out[i] = z0gmOBXiFbdNSicImCIt1( x[ seq(i, i+n-1) ], weights );
  }
  
  return out;
  
}

// [[Rcpp::export]]
NumericMatrix z0gmOBXiFbdNSicImCIt1_matrix( NumericMatrix A, int n, bool by_column, NumericVector weights ) {
  
  int nRow = A.nrow();
  int nCol = A.ncol();
  if( by_column ) {
    
    NumericMatrix B( nRow - n + 1, nCol );
    
    for( int j=0; j < nCol; j++ ) {
      
      NumericVector tmp = A(_, j);
      for( int i=0; i < nRow - n + 1; i++ ) {
        B(i, j) = z0gmOBXiFbdNSicImCIt1( tmp[ seq(i, i+n-1) ], weights );
      }
    }
    
    return B;
    
  } else {
    
    NumericMatrix B( nRow, nCol - n + 1 );
    
    for( int i=0; i < nRow; i++ ) {
      
      NumericVector tmp = A(i, _);
      for( int j=0; j < nCol - n + 1; j++ ) {
        B(i, j) = z0gmOBXiFbdNSicImCIt1( tmp[ seq(j, j+n-1) ], weights );
      }
    }
    
    return B;
    
  }
  
}
