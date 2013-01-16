roll_RcppArmadillo_file <- function( fun, includes, depends, types ) {
  
  outFile <- file.path( getwd(), "src", "source_files.cpp" )
  conn <- file( outFile, open="a" )
  
  w <- function(...) {
    cat( paste0(..., "\n"), file=conn)
  }
  
  w1 <- function(...) {
    cat( paste0("\t", ..., "\n"), file=conn)
  }
  
  w2 <- function(...) {
    cat( paste0("\t\t", ..., "\n"), file=conn)
  }
  
  w3 <- function(...) {
    cat( paste0("\t\t\t", ..., "\n"), file=conn)
  }
  
  w4 <- function(...) {
    cat( paste0("\t\t\t\t", ..., "\n"), file=conn)
  }
  
  if( "NumericVector" %in% types ) {
    
    ## write out the numeric vector handler
    w("// [[Rcpp::export]]")
    w( "NumericVector roll_", fun, "_numeric_vector( arma::vec x, int n, arma::vec weights ) {" )
    w1()
    w1("int len = x.n_elem;")
    w1("int len_out = len - n + 1;")
    w1()
    w1("arma::vec out( len_out );")
    w1()
    w1("for( int i=0; i < len_out; i++ ) {")  
    w2("out(i) = arma::", fun, "( x.subvec(i, i+n-1) % weights );")
    w1("}")
    w1()
    w1("return wrap(out);")
    w1()
    w("}")
    w()
    
  }
  
  if( "NumericMatrix" %in% types ) {
    
    ## write out the matrix handler
    w("// [[Rcpp::export]]")
    w("NumericMatrix roll_", fun, "_numeric_matrix( arma::mat A, int n, bool by_column, arma::vec weights ) {")
    w1()
    w1("int nRow = A.n_rows;")
    w1("int nCol = A.n_cols;")
    w1()
    w1("if( by_column ) {")
    w2("arma::mat B( nRow - n + 1, nCol );")
    w2()
    w2("for( int j=0; j < nCol; j++ ) {")
    w3()
    w3("arma::colvec tmp = A.col(j);")
    w3("for( int i=0; i < nRow - n + 1; i++ ) {")
    w4("B(i, j) = arma::", fun, "( tmp.subvec( i, i+n-1 ) % weights );")
    w3("}")
    w2("}")
    w2()
    w2("return wrap(B);")
    w1()
    w1("} else {")
    w2()
    w2("arma::mat B( nRow, nCol - n + 1 );")
    w2()
    w2("for( int i=0; i < nRow; i++ ) {")
    w3()
    w3("arma::rowvec tmp = A.row(i);")
    w3("for( int j=0; j < nCol - n + 1; j++ ) {")
    w4("B(i, j) = arma::", fun, "( tmp.subvec( j, j+n-1 ) );")
    w3("}")
    w2("}")
    w2()
    w2("return wrap(B);")
    w1()
    w1("}")
    w()
    w("}")
    
  }
  
  close( conn )
  
}