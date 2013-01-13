#' Generate your own Weighted C++ Roll Function
#' 
#' Using this interface, you can define a function that you would like to
#' be called on each sub-vector you are rolling over. The generated code is
#' exposed via \code{sourceCpp}.
#' 
#' By default, we include \code{<Rcpp.h>} in each file; however, you can
#' include your own libraries with the \code{includes} call.
#' 
#' @param fun A character string defining the function call. The function must be in 
#' terms of variable \code{x}. The function will be applied individually 
#' to each element being 'roll'ed over, unless \code{vector=TRUE} is set.
#' @param vector boolean; if \code{TRUE}, the function supplied attempts to take
#' the entire vector passed, and returns a scalar result. Otherwise, we
#' apply the function to each element of the vector \code{x} individually.
#' @param const_vars Constant variables you would like to 'live' within
#' the sourced C++ function. Format is a named \code{list}; eg, you
#' could pass \code{list(pi=pi)} to have \code{pi} as a constant variable
#' available in the function you're calling. You can also supply any
#' constant scalar-valued function of a vector; eg. \code{list(n="x.size()")} 
#' will assign \code{n} to the length of the sub-vector \code{x}.
#' @param combine character; must be one of \code{"+", "-", "*", "/"}. 
#' how should we combine elements of the vector we are collapsing over?
#' @param final_trans A final transformation to perform after 'rolling'
#' over each element in the vector \code{x}.
#' @param includes Other C++ libraries to include. For example, to include
#' \code{boost/math.hpp}, you would pass
#' \code{c("boost/math.hpp")}.
#' @param depends Other libraries to link to. Linking is done through
#' Rcpp attributes.
#' @param inline boolean; mark the function generated as \code{inline}?
#' This may or may not increase execution speed.
#' @param ... Additional arguments passed to \code{sourceCpp}.
#' @return A wrapper \R function to compiled C++ files, as generated through
#' \code{sourceCpp}. The function accepts arguments for:
#' a vector/matrix \code{x}, a window-size \code{n}, the logical \code{by.column},
#' a vector of \code{weights}, and a \code{by} statement. 
#' Note that the \code{weights} provides are un-normalized.
#' By is used to 'skip' entries; so that setting \code{by=2} and a
#' window size \code{n} of 3 would effectively call the function on only
#' the first and third elements of each vector passed through.
#' @export
#' @seealso \code{\link{sourceCpp}} for information on how Rcpp
#' compiles your functions, and \code{\link{get_rollit_source}} for
#' inspection of the generated C++ code.
#' @note All functions generated use Rcpp's \code{NumericVector} and
#' \code{NumericMatrix} to interface to \R vectors and matrices. Because of
#' this, any function that you would like to call on the whole vector being
#' rolled over (eg, if you choose \code{vector=TRUE}) needs to be compatible 
#' with the Rcpp \code{NumericVector}. Elements within these vectors are 
#' translated as \code{double}s so any function that receives a \code{double} 
#' will work fine (eg, if you choose \code{vector=FALSE}).
#' 
#' @examples \dontrun{
#' x <- matrix(1:16, nrow=4)
#' 
#' ## the squared rolling sum -- we square the sum of our rolled results
#' rolling_sqsum <- rollit( final_trans="x*x" )
#' 
#' rolling_sqsum( x, 4 )
#' rolling_sqsum( x, 4, by.column=FALSE )
#' cbind( as.vector(rolling_sqsum(x, 4)), apply(x, 2, function(x) { sum(x)^2 } ) )
#' 
#' ## use the Rcpp sugar function 'mean' -- let's compute a 'sqrt mean'
#' rolling_mean <- rollit( "mean(x)", vector=TRUE, final_trans="sqrt( x )" )
#' 
#' ## implement your own variance function
#' ## we can use the sugar function 'mean' to get
#' ## the mean of x
#' 
#' const_vars <- list(m = "mean(x)", n = "x.size()")
#' var_fun <- "( (x-m) * (x-m) )/(n-1)"
#' rolling_var <- rollit( var_fun, const_vars=const_vars )
#' 
#' x <- c(1, 5, 10, 15)
#' cbind( rolling_var(x, 2), roll_var(x, 2) )
#' 
#' ## use a function from cmath
#' 
#' rolling_log10 <- rollit( "log10(x)" )
#' rolling_log10( 10^(1:5), 2 )
#' 
#' ## rolling product
#' rolling_prod <- rollit( combine="*" )
#' rolling_prod( 1:10, 2 )
#' 
#' ## a benchmark
#' 
#' if( require("microbenchmark") && require("zoo") ) {
#'   x <- rnorm(1E4)
#'   microbenchmark( 
#'     rolling_var(x, 100),
#'     roll_var(x, 100),
#'     rollapply(x, 100, var),
#'     times=10
#'     )
#'   }}
rollit <- function( fun="x",
                    vector=FALSE,
                    const_vars=NULL,
                    combine="+",
                    final_trans=NULL,
                    includes=NULL, 
                    depends=NULL,
                    inline=TRUE,
                    ... ) {
  
  ## error checks
  if( !is.null(const_vars) ) {
    if( !is.list(const_vars) || is.list(const_vars) && is.null( names( const_vars ) ) ) {
      stop("'const_vars' must be a named list")
    }
  }
  
  if( length( combine ) > 1 || !(combine %in% c("+", "-", "*", "/") ) ) {
    stop("combine must be one of '+', '-', '*', '/'")
  }
  
  funky_regex <- "([^a-zA-Z_])(x)(?=[^x])|(\\Ax)|(x\\z)"
  if( length( grep( funky_regex, fun, perl=TRUE ) ) < 1 ) {
    stop("'fun' must be in terms of a variable 'x'")
  }
  
  ## random name
  name <- paste( sep="", collapse="", c("z",
                                        sample( c( letters, LETTERS, 0:9), size=20, replace=TRUE )
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
  
  if( inline) w("inline")
  w("double ", name, "(NumericVector x, const NumericVector weights) {")
  if( combine %in% c("+", "-") ) { 
    w1("double out_ = 0;")
  } else {
    w1("double out_ = 1;")
  }
  
  ## constant variables
  for( i in seq_along(const_vars) ) {
    if( !is.null(const_vars) ) {
      w1("const double ", names(const_vars)[i], " = ", const_vars[i], ";")
    }
  }
  
  ## funky parser
  (parsed_fun <- gsub( funky_regex, "\\1\\2\\3\\4\\5[i]", fun, perl=TRUE ))
  
  ## apply function elementwise
  if( !vector ) {
    w1("for( int i=0; i < x.size(); i++ ) {")
    w2("out_ ", combine, "= weights[i] * ", parsed_fun, ";")
    w1("}")
  } else {
    w1("out_ = ", fun, ";")
  }
  if( !is.null( final_trans ) ) {
    w1("out_ = ", gsub( funky_regex, "\\1out_", final_trans, perl=TRUE), ";" )
  }
  w1("return out_;")
  w("}")
  w()
  
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
  
  return( function(x, n, by.column=TRUE, weights=rep(1, n), by=1 ) {
    
    outFile <- outFile
    
    if( by > 1 ) {
      weights[ 1:length(weights) %% by != 0 ] <- 0
    }
    
    if( is.matrix(x) ) {
      if( n > nrow(x) ) {
        stop("n cannot be greater than nrow(x).")
      }
      call <- call( paste( sep="", name, "_matrix" ), x, as.integer(n), as.logical(by.column), as.numeric(weights) )
      return( eval( call, envir=cpp_env ) )
    }
    
    if( is.vector(x) ) {
      if( n > length(x) ) {
        stop("n cannot be greater than length(x).")
      }
      call <- call( paste( sep="", name, "_numeric" ), x, as.integer(n), as.numeric(weights) )
      return( as.numeric( eval( call, envir=cpp_env ) ) )
    }
    
    stop("the x supplied is neither a vector or a matrix")
    
  } )
  
}