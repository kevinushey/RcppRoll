#include <Rcpp.h>
using namespace Rcpp;

const double flagval = __DBL_MIN__;
inline double flag(double a, bool b) { return b ? a : flagval; }

// [[Rcpp::export]]
NumericVector subsetter(NumericVector a, LogicalVector b) {
  
  transform(a.begin(), a.end(), b.begin(), a.begin(), flag);
  NumericVector res = NumericVector(sum(b));
  remove_copy(a.begin(), a.end(), res.begin(), flagval);
  return res;    

}

// [[Rcpp::export]]
NumericVector roll_mean_numeric_vector( NumericVector x, int n, NumericVector weights ) {
  
  int len = x.size();
  int len_out = len - n + 1;
  
  NumericVector out = no_init( len_out );
  
  for( int i=0; i < len_out; i++ ) {
    out[i] = mean( x[ seq(i, i+n-1) ] * weights );
  }
  
  return out;
  
}

// [[Rcpp::export]]
NumericMatrix roll_mean_numeric_matrix( NumericMatrix A, int n, bool by_column, NumericVector weights ) {
  
  int nRow = A.nrow();
  int nCol = A.ncol();
  if( by_column ) {
    
    NumericMatrix B( nRow - n + 1, nCol );
    
    for( int j=0; j < nCol; j++ ) {
      
      NumericVector tmp = A(_, j);
      for( int i=0; i < nRow - n + 1; i++ ) {
        B(i, j) = mean( tmp[ seq(i, i+n-1) ] );
      }
    }
    
    return B;
    
  } else {
    
    NumericMatrix B( nRow, nCol - n + 1 );
    
    for( int i=0; i < nRow; i++ ) {
      
      NumericVector tmp = A(i, _);
      for( int j=0; j < nCol - n + 1; j++ ) {
        B(i, j) = mean( tmp[ seq(j, j+n-1) ] );
      }
    }
    
    return B;
    
  }
  
}
