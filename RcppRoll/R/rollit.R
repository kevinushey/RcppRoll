#' Generate your own C++ Roll Function
#' 
#' Using this interface, you can define a function that you would like to
#' be called on each sub-vector you are rolling over. The generated code is
#' exposed via \code{sourceCpp}.
#' 
#' By default, we include \code{<Rcpp.h>} in each file; however, you can
#' include your own libraries with the \code{includes} call.
#' 
#' @param name a name to use for the function.
#' @param fun the function call. must be in terms of variable \code{x}.
#' @param combine character; must be one of \code{"+", "-", "*", "/"}. 
#' how should we combine elements of the vector we are collapsing over?
#' @param final_trans a final transformation to perform after 'rolling'
#' over each element in the vector \code{x}.
#' @param const_vars constant variables you would like to 'live' within
#' the sourced C++ function. format is a named \code{list}; eg, you
#' could pass \code{list(pi=pi)} to have \code{pi} as a constant variable
#' referencable in the function you're calling.
#' @param includes other C++ libraries to include. for example, to include
#' \code{boost/math.hpp}, you would pass
#' \code{c("boost/math.hpp")}.
#' @param depends other libraries to link to; linking is done through
#' Rcpp attributes.
#' @param ... additional arguments passed to \code{sourceCpp}.
#' 
#' This is then wrapped through a more generic 'rolling' sum function.
#' 
#' 
#' @return A wrapper \R function of the same name as \code{name}.
#' @export
#' @examples
#' 
#' ## implement your own variance function
#' ## we can use the sugar function 'mean' to get
#' ## the mean of x
#' 
#' const_vars <- list(m = "mean(x)", n = "x.size()")
#' var_fun <- "pow( (x - m), 2)/(n-1)"
#' rollit( "rolling_var", var_fun, const_vars=const_vars )
#' 
#' x <- c(1, 5, 10, 15)
#' cbind( rolling_var(x, 2), roll_var(x, 2) )
#' 
#' ## use a function from cmath
#' 
#' rolling_ex <- rollit( "rolling_ex",
#'   "log10(x)"
#'   )
#' 
#' ## your C++ rolled functions will be much faster than an R
#' ## apply equivalent, but perhaps not as fast as one of the
#' ## internal versions...
#' 
#' if( require("microbenchmark") && require("zoo") ) {
#'   x <- rnorm(1E4)
#'   microbenchmark( 
#'     rolling_var(x, 100),
#'     roll_var(x, 100),
#'     rollapply(x, 100, var),
#'     times=10
#'     )
#'   }
rollit <- function( name, 
                    fun="x",
                    combine="+", 
                    final_trans=NULL,
                    const_vars=NULL, 
                    includes=NULL, 
                    depends=NULL,
                    ... ) {
  
  ## environment for cppSource generated files
  cpp_env <- new.env()
  
  if( length( combine ) > 1 || !(combine %in% c("+", "-", "*", "/") ) ) {
    stop("combine must be one of '+', '-', '*', '/'")
  }
  
  if( exists(name) ) {
    cat("Warning! A variable is already bound to", deparse( substitute( name ) ), ".",
        "\nReplace this binding? (y/n):\n")
    if( length( grep( "^y", scan( quiet=TRUE, what=character(), n=1 ) ) ) < 1 ) {
      cat("Function processing stopped.")
      return( get(name) )
    }
  }
  
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
  
  w("double ", name, "(NumericVector x) {")
  w1("double out = 0;")
  
  ## constant variables
  for( i in seq_along(const_vars) ) {
    if( !is.null(const_vars) ) {
      w1("double ", names(const_vars)[i], " = ", const_vars[i], ";")
    }
  }
  
  ## funky parser
  funky_regex <- "([^a-zA-Z_])(x)(?=[^x])|(^x$)"
  (parsed_fun <- gsub( funky_regex, "\\1\\2\\3[i]", fun, perl=TRUE ))
  
  w1("for( int i=0; i < x.size(); i++ ) {")
  w2("out ", combine, "= (", parsed_fun, ");")
  w1("}")
  if( !is.null( final_trans ) ) {
    w1("out = ", gsub( funky_regex, "\\1out", final_trans, perl=TRUE), ";" )
  }
  w1("return out;")
  w("}")
  w()
  
  ## numericvector call
  w("// [[Rcpp::export]]")
  w( "NumericVector ", name, "_numeric( NumericVector x, int n ) {" )
  w1()
  w1("int len = x.size();")
  w1("int len_out = len - n + 1;")
  w1()
  w1("NumericVector out( len_out );")
  w1()
  w1("for( int i=0; i < len_out; i++ ) {")  
  w2("out[i] = ", name, "( x[ seq(i, i+n-1) ] );")
  w1("}")
  w1()
  w1("return out;")
  w1()
  w("}")
  w()
  
  ## function definition -- matrix
  
  w("// [[Rcpp::export]]")
  w("NumericMatrix ", name, "_matrix( NumericMatrix A, int n ) {")
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
  w3("B(i, j) = ", name, "( tmp[ seq(i, i+n-1) ] );")
  w2("}")
  w1("}")
  w1()
  w1("return B;")
  w1()
  w("}")
  
  cat("C++ source file written to", outFile, ".")
  
  sourceCpp( outFile, env=cpp_env, ... )
  
  return( function(x, n) {
    
    if( is.matrix(x) ) {
      if( n > nrow(x) ) {
        stop("n cannot be greater than nrow(x).")
      }
      call <- call( paste( sep="", name, "_matrix" ), x, as.integer(n) )
      return( eval( call, envir=cpp_env ) )
    }
    
    if( is.vector(x) ) {
      if( n > length(x) ) {
        stop("n cannot be greater than length(x).")
      }
      call <- call( paste( sep="", name, "_numeric" ), x, as.integer(n) )
      return( as.numeric( eval( call, envir=cpp_env ) ) )
    }
    
    stop("the x supplied is neither a vector or a matrix")
    
  } )
  
}