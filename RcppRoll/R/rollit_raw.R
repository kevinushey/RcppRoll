#' Wrap in a Raw C++ Function
#' 
#' Using this, you can write and wrap in your own C++ function. The onus is,
#' however, on you to know what you're doing.
#' 
#' The signature of the function is fixed as:
#' 
#' \code{double <name>( NumericVector x, const NumericVector weights )}
#' 
#' which are passed in from the generated \R function.
#' 
#' @param fun A character string defining the function call. See examples
#' for usage.
#' @param includes Other C++ libraries to include. For example, to include
#' \code{boost/math.hpp}, you would pass
#' \code{c("boost/math.hpp")}.
#' @param depends Other libraries to link to. Linking is done through
#' Rcpp attributes.
#' @param inline boolean; mark this function as inline? This may or may not
#' increase execution speed.
#' @param ... Optional arguments passed to \code{sourceCpp}.
#' @export
#' @examples \dontrun{
#' ## implement a weighted rolling 'sum of squares'
#' fun <- "
#' double out = 0;
#' for( int i=0; i < x.size(); i++ ) {
#'   out += weights[i] * x[i] * x[i];
#'   }
#' return( out );
#' "
#' rolling_sumsq <- rollit_raw( fun )
#' x <- 1:5
#' rolling_sumsq( x, 5 ) == sum( x^2 )
#' }
rollit_raw <- function( fun,
                        depends=NULL,
                        includes=NULL,
                        inline=TRUE,
                        ...) {
  
  ## random name
  name <- paste( 
    sep="", collapse="", 
    c("z", sample( c( letters, LETTERS, 0:9), size=20, replace=TRUE )
    ) )
  
  ## environment for cppSource generated files
  cpp_env <- new.env()
  
  outFile <- paste( sep="", tempfile(), ".cpp" )
  conn <- file( outFile, open="w" )
  on.exit( close(conn) )
  
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
  
  ## depends
  if( !is.null(depends) ) {
    x <- paste( depends, collapse=", ")
    w("// [[Rcpp::depends(", x, ")")
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
  
  ## namespace
  w("using namespace Rcpp;")
  w()
  
  ## wrap the function provided by the user
  if( inline ) w("inline")
  w("double ", name, "( NumericVector x, NumericVector weights ) {")
  w(fun)
  w("}")
  
  ## numericvector call
  w("// [[Rcpp::export]]")
  w( "NumericVector ", name, "_numeric( NumericVector x, int n, const NumericVector weights ) {" )
  w1()
  w1("int len = x.size();")
  w1("int len_out = len - n + 1;")
  w1()
  w1("NumericVector out = no_init( len_out );")
  w1()
  w1("for( int i=0; i < len_out; i++ ) {") 
  w2("out[i] = ", name, "( x[ seq(i, i+n-1) ], weights );")
  w1("}")
  w1()
  w1("return out;")
  w1()
  w("}")
  w()
  
  ## function definition -- matrix
  
  w("// [[Rcpp::export]]")
  w("NumericMatrix ", name, "_matrix( NumericMatrix A, int n, bool by_column, const NumericVector weights ) {")
  w1()
  w1("int nRow = A.nrow();")
  w1("int nCol = A.ncol();")
  
  ## by column
  w1("if( by_column ) {")
  w2()
  w2("NumericMatrix B( nRow - n + 1, nCol );")
  w2()
  w2("for( int j=0; j < nCol; j++ ) {")
  w3()
  w3("NumericVector tmp = A(_, j);")
  w3("for( int i=0; i < nRow - n + 1; i++ ) {")
  w4("B(i, j) = ", name, "( tmp[ seq(i, i+n-1) ], weights );")
  w3("}")
  w2("}")
  w2()
  w1("return B;")
  w1()
  
  ## by row
  w1("} else {")
  w2()
  w2("NumericMatrix B( nRow, nCol - n + 1 );")
  w2()
  w2("for( int i=0; i < nRow; i++ ) {")
  w3()
  w3("NumericVector tmp = A(i, _);")
  w3("for( int j=0; j < nCol - n + 1; j++ ) {")
  w4("B(i, j) = ", name, "( tmp[ seq(j, j+n-1) ], weights );")
  w3("}")
  w2("}")
  w2()
  w1("return B;")
  w1()
  w1("}")
  w()
  w("}")
  
  
  cat("C++ source file written to", outFile, ".\n")
  cat("Compiling...\n")
  sourceCpp( outFile, env=cpp_env, ... )
  cat("Done!\n")
  
  return( function(x, n, by.column=TRUE, weights=rep(1,n), normalize=FALSE ) {
    
    force( outFile )
    
    if( length(weights) != n ) {
      stop("length of weights must equal n")
    }
    
    if( normalize ) {
      weights <- weights * length(weights) / sum(weights)
    }
    
    if( is.matrix(x) ) {
      if( n > nrow(x) ) {
        stop("n cannot be greater than nrow(x)")
      }
      call <- call( paste( sep="", name, "_matrix" ), 
                    x, 
                    as.integer(n), 
                    as.logical(by.column), 
                    as.numeric(weights)
      )
      return( eval( call, envir=cpp_env ) )
    }
    
    if( is.vector(x) ) {
      if( n > length(x) ) {
        stop("n cannot be greater than length(x)")
      }
      call <- call( paste( sep="", name, "_numeric" ), 
                    x, 
                    as.integer(n), 
                    as.numeric(weights)
      )
      return( as.numeric( eval( call, envir=cpp_env ) ) )
    }
    
    stop("the x supplied is neither a vector nor a matrix")
    
  } )
  
}