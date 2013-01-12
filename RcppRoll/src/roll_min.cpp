#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector roll_min_numeric_vector( NumericVector x, int n ) {
	
	int len = x.size();
	int len_out = len - n + 1;
	
	NumericVector out = no_init( len_out );
	
	for( int i=0; i < len_out; i++ ) {
		out[i] = min( x[ seq(i, i+n-1) ] );
	}
	
	return out;
	
}

// [[Rcpp::export]]
NumericMatrix roll_min_numeric_matrix( NumericMatrix A, int n, bool by_column ) {
	
	int nRow = A.nrow();
	int nCol = A.ncol();
	if( by_column ) {
		
		NumericMatrix B( nRow - n + 1, nCol );
		
		for( int j=0; j < nCol; j++ ) {
			
			NumericVector tmp = A(_, j);
			for( int i=0; i < nRow - n + 1; i++ ) {
				B(i, j) = min( tmp[ seq(i, i+n-1) ] );
			}
		}
	
	return B;
	
	} else {
	
		NumericMatrix B( nRow, nCol - n + 1 );
		
		for( int i=0; i < nRow; i++ ) {
			
			NumericVector tmp = A(i, _);
				for( int j=0; j < nCol - n + 1; j++ ) {
			B(i, j) = min( tmp[ seq(j, j+n-1) ] );
			}
		}
	
	return B;
	
	}

}
