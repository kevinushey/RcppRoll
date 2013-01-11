// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector roll_median_numeric_vector( NumericVector x_, int n ) {
	
	arma::vec x = as<arma::vec>(x_);
	
	int len = x.size();
	int len_out = len - n + 1;
	
	arma::vec out( len_out );
	
	for( int i=0; i < len_out; i++ ) {
		out(i) = arma::median( x.subvec(i, i+n-1) );
	}
	
	return wrap(out);
	
}

// [[Rcpp::export]]
NumericMatrix roll_median_numeric_matrix( NumericMatrix A_, int n ) {
	
	arma::mat A = as<arma::mat>(A_);
	
	int nRow = A.n_rows;
	int nCol = A.n_cols;
	
	arma::mat B( nRow - n + 1, nCol );
	
	for( int j=0; j < nCol; j++ ) {
		
		arma::vec tmp = A.col(j);
		for( int i=0; i < nRow - n + 1; i++ ) {
			B(i, j) = arma::median( tmp.subvec( i, i+n-1 ) );
		}
	}
	
	return wrap(B);
	
}
