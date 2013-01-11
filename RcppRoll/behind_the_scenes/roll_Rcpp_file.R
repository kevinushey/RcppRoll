roll_Rcpp_file <- function( fun, includes, depends, types ) {
  
  outFile <- file.path( getwd(), "src", paste( sep="", "roll_", fun, ".cpp" ) )
  conn <- file( outFile, open="w" )
  
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
  
  ## Write out dependencies
  if( !is.null(depends) ) {
    w( paste0("// [[Rcpp::depends(", 
              paste( depends, collapse=", " ),
              ")]]") )
  }  
  
  ## Write out the includes
  if( !("RcppArmadillo" %in% depends) ) {
    w("#include <Rcpp.h>")
  }
  if( !is.null(includes) ) {
    for( include in includes ) {
      w( paste0("#include <", include, ">") )
      w()
    }
  }
  w("using namespace Rcpp;")
  w()
  
  if( "NumericVector" %in% types ) {
    
    ## write out the numeric vector handler
    w("// [[Rcpp::export]]")
    w( "NumericVector roll_", fun, "_numeric_vector( NumericVector x, int n ) {" )
    w1()
    w1("int len = x.size();")
    w1("int len_out = len - n + 1;")
    w1()
    w1("NumericVector out = no_init( len_out );")
    w1()
    w1("for( int i=0; i < len_out; i++ ) {")  
    w2("out[i] = ", fun, "( x[ seq(i, i+n-1) ] );")
    w1("}")
    w1()
    w1("return out;")
    w1()
    w("}")
    w()
    
  }
  
  if( "NumericMatrix" %in% types ) {
    
    ## write out the matrix handler
    w("// [[Rcpp::export]]")
    w("NumericMatrix roll_", fun, "_numeric_matrix( NumericMatrix A, int n ) {")
    w1()
    w1("int nRow = A.nrow();")
    w1("int nCol = A.ncol();")
    w1()
    w1("NumericMatrix B( nRow - n + 1, nCol );")
    w1()
    w1("for( int j=0; j < nCol; j++ ) {")
    w2()
    w2("NumericVector tmp = A(_, j);")
    w2("for( int i=0; i < nRow - n + 1; i++ ) {")
    w3("B(i, j) = ", fun, "( tmp[ seq(i, i+n-1) ] );")
    w2("}")
    w1("}")
    w1()
    w1("return B;")
    w1()
    w("}")
    
  }
  
  if( "CharacterVector" %in% types ) {
    
    ## write out the numeric vector handler
    w("// [[Rcpp::export]]")
    w( "CharacterVector roll_", fun, "_character_vector( CharacterVector x, int n ) {" )
    w1()
    w1("int len = x.size();")
    w1("int len_out = len - n + 1;")
    w1()
    w1("CharacterVector out = no_init( len_out );")
    w1()
    w1("for( int i=0; i < len_out; i++ ) {")  
    w2("out[i] = ", fun, "( x[ seq(i, i+n-1) ] );")
    w1("}")
    w1()
    w1("return out;")
    w1()
    w("}")
    w()
    
  }
  
  if( "CharacterMatrix" %in% types ) {
    
    ## write out the matrix handler
    w("// [[Rcpp::export]]")
    w("CharacterMatrix roll_", fun, "_character_marix( CharacterMatrix A, int n ) {")
    w1()
    w1("int nRow = A.nrow();")
    w1("int nCol = A.ncol();")
    w1()
    w1("CharacterMatrix B( nRow - n + 1, nCol );")
    w1()
    w1("for( int j=0; j < nCol; j++ ) {")
    w2()
    w2("CharacterVector tmp = A(_, j);")
    w2("for( int i=0; i < nRow - n + 1; i++ ) {")
    w3("B(i, j) = ", fun, "( tmp[ seq(i, i+n-1) ] );")
    w2("}")
    w1("}")
    w1()
    w1("return B;")
    w1()
    w("}")
    
  }
  
  close( conn )
  
}