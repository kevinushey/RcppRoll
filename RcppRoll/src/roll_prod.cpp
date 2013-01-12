// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector roll_prod_numeric_vector( NumericVector x_, int n ) {
	
	arma::vec x = as<arma::vec>(x_);
	
	int len = x.size();
	int len_out = len - n + 1;
	
	arma::vec out( len_out );
	
	for( int i=0; i < len_out; i++ ) {
		out(i) = arma::prod( x.subvec(i, i+n-1) );
	}
	
	return wrap(out);
	
}

// [[Rcpp::export]]
NumericMatrix roll_prod_numeric_matrix( NumericMatrix A_, int n, bool by_column ) {
	
	int nRow = A_.nrow();
	int nCol = A_.ncol();
	
	arma::mat A(A_.begin(), nRow, nCol, false);
	
	if( by_column ) {
		arma::mat B( nRow - n + 1, nCol );
		
		for( int j=0; j < nCol; j++ ) {
			
			arma::colvec tmp = A.col(j);
			for( int i=0; i < nRow - n + 1; i++ ) {
				B(i, j) = arma::prod( tmp.subvec( i, i+n-1 ) );
			}
		}
		
		return wrap(B);
	
	} else {
		
		arma::mat B( nRow, nCol - n + 1 );
		
		for( int i=0; i < nRow; i++ ) {
			
			arma::rowvec tmp = A.row(i);
			for( int j=0; j < nCol - n + 1; j++ ) {
				B(i, j) = arma::prod( tmp.subvec( j, j+n-1 ) );
			}
		}
		
		return wrap(B);
	
	}

}
