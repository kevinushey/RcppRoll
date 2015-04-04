#' Generate your own Weighted C++ Roll Function
#'
#' Using this, you can write and wrap in your own C++ function.
#'
#' The signature of \code{fun} is fixed as:
#'
#' \code{double <name>( NumericVector& x, NumericVector& weights, const int& n, const int& N, const int& ind)}
#'
#' where
#'
#' \itemize{
#'
#' \item{\code{X_SUB} is a \code{#define} macro that expands to the sub-vector being rolled over,}
#'
#' \item{\code{X(i)} is a \code{#define} macro that expands to the current element of \code{X_SUB}
#' in a loop being rolled over,}
#'
#' \item{\code{x} is a reference to the \bold{entire} vector (not just the
#' sub-vector being rolled over),}
#'
#' \item{\code{weights} are the weights,}
#'
#' \item{\code{n} is the window size,}
#'
#' \item{\code{N} is the number of non-zero \code{weights} passed,}
#'
#' \item{\code{ind} is the current position along vector \code{x}.}
#' }
#'
#' Because the variables are being passed by reference, you
#' should \bold{not} modify them, unless you're prepared for strange behavior.
#' See examples for a potential usage.
#'
#' @param fun A character string defining the function call. See examples
#' for usage.
#' @param includes Other C++ libraries to include. For example, to include
#' \code{boost/math.hpp}, you would pass
#' \code{c("<boost/math.hpp>")}.
#' @param depends Other libraries to link to. Linking is done through
#' Rcpp attributes.
#' @param inline boolean; mark this function as inline? This may or may not
#' increase execution speed.
#' @param name string; a name to internally assign to your generated C++ functions.
#' @param additional Other C++ code you want to include; e.g. helper functions.
#' This code will be inserted as-is above the code in \code{fun}.
#' @param ... Optional arguments passed to \code{sourceCpp}.
#' @export
#' @examples \dontrun{
#' ## implement a weighted rolling 'sum of squares'
#' fun <- "
#' double out = 0;
#' const double m = mean( X_SUB );
#' for( int i=0; i < n; i++ ) {
#'   out += weights[i] * ( (X(i)-m) * (X(i)-m) ) / (N-1);
#'   }
#' return out;
#' "
#'
#' rolling_var <- rollit_raw( fun )
#' x <- 1:5
#' rolling_var( x, 5 ) == var(x)
#'
#' ## a (slow-ish) implementation of rolling kurtosis
#' fun <- "
#' double numerator = 0;
#' double denominator = 0;
#' const double m = mean( X_SUB );
#' for( int i=0; i < n; i++ ) {
#'   double tmp = ( X(i) - m ) * ( X(i) - m );
#'   numerator += tmp * tmp;
#'   denominator += tmp;
#' }
#' return N * numerator / ( denominator * denominator );
#' "
#'
#' rolling_kurt <- rollit_raw( fun )
#' x <- rnorm(100)
#' rolling_kurt(x, 20)
#' }
rollit_raw <- function(fun,
                       depends = NULL,
                       includes = NULL,
                       inline = TRUE,
                       name = NULL,
                       additional = NULL,
                       ...) {
  .Deprecated()

  ## random name if null
  if (is.null(name)) {
    random_string <- sample(c(letters, LETTERS, 0:9), 20, TRUE)
    name <- paste(sep = "", collapse = "", c("z", random_string))
  }

  ## environment for cppSource generated files
  cpp_env <- new.env()

  outFile <- paste(sep = "", tempfile(), ".cpp")
  conn <- file(outFile, open = "w")
  on.exit(close(conn))

  w <- function(...) {
    cat(paste0(..., "\n"), file = conn)
  }

  w1 <- function(...) {
    cat(paste0("\t", ..., "\n"), file = conn)
  }

  w2 <- function(...) {
    cat(paste0("\t\t", ..., "\n"), file = conn)
  }

  w3 <- function(...) {
    cat(paste0("\t\t\t", ..., "\n"), file = conn)
  }

  w4 <- function(...) {
    cat(paste0("\t\t\t\t", ..., "\n"), file = conn)
  }

  ## depends
  if (is.null(depends)) {
    w("// [[Rcpp::depends(RcppArmadillo)]]")
  } else {
    w("// [[Rcpp::depends(RcppArmadillo, ",
      paste(depends, collapse = ", "),
      ")")
  }

  w("#include <RcppArmadillo.h>")
  if (!is.null(includes)) {
    for(include in includes) {
      w(paste0("#include ", include))
      w()
    }
  }

  ## defines
  w("#ifndef X_SUB")
  w("#define X_SUB (x[ seq(ind, ind+n-1) ])")
  w("#endif")
  w()
  w("#ifndef X(i)")
  w("#define X(i) (x[i+ind])")
  w("#endif")
  w()

  ## namespace
  w("using namespace Rcpp;")
  w()

  w(additional)
  w()

  ## wrap the function provided by the user
  if (inline)
    w("inline")
  w(
    "double ", name, "(NumericVector& x, NumericVector& weights, const int& n, const int& N, const int& ind) {"
  )
  w(fun)
  w("}")

  ## numericvector call
  w("// [[Rcpp::export]]")
  w("NumericVector ", name, "_numeric( NumericVector x, int n, NumericVector weights ) {")
  w1()
  w1("int len = x.size();")
  w1("int len_out = len - n + 1;")
  w1("int N = sum( sign( weights*weights ) );")
  w1()
  w1("NumericVector out = no_init( len_out );")
  w1()
  w1("for( int ind=0; ind < len_out; ind++ ) {")
  w2()
  w2("out[ind] = ", name, "(x, weights, n, N, ind );")
  w1("}")
  w1()
  w1("return out;")
  w1()
  w("}")
  w()

  ## function definition -- matrix

  w("// [[Rcpp::export]]")
  w(
    "NumericMatrix ", name, "_matrix( NumericMatrix A, int n, bool by_column, NumericVector weights ) {"
  )
  w1()
  w1("int nRow = A.nrow();")
  w1("int nCol = A.ncol();")
  w1("int N = sum( sign( weights*weights ) );")

  ## by column
  w1("if( by_column ) {")
  w2()
  w2("NumericMatrix B( nRow - n + 1, nCol );")
  w2()
  w2("for( int j=0; j < nCol; j++ ) {")
  w3()
  w3("NumericVector tmp = A(_, j);")
  w3("for( int ind=0; ind < nRow - n + 1; ind++ ) {")
  w4()
  w4("B(ind, j) = ", name, "( tmp, weights, n, N, ind );")
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
  w3("for( int ind=0; ind < nCol - n + 1; ind++ ) {")
  w4()
  w4("B(i, ind) = ", name, "( tmp, weights, n, N, ind );")
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
  sourceCpp(outFile, env = cpp_env, ...)
  cat("Done!\n")

  return(function(x, n, by.column = TRUE, weights = rep(1,n), normalize =
                    FALSE) {
    force(outFile)

    if (length(weights) != n) {
      stop("length of weights must equal n")
    }

    if (normalize) {
      weights <- weights * length(weights) / sum(weights)
    }

    if (is.matrix(x)) {
      if (n > nrow(x)) {
        stop("n cannot be greater than nrow(x)")
      }
      call <- call(
        paste(sep = "", name, "_matrix"),
        x,
        as.integer(n),
        as.logical(by.column),
        as.numeric(weights)
      )
      return(eval(call, envir = cpp_env))
    }

    if (is.vector(x)) {
      if (n > length(x)) {
        stop("n cannot be greater than length(x)")
      }
      call <- call(paste(sep = "", name, "_numeric"),
                   x,
                   as.integer(n),
                   as.numeric(weights))
      return(as.numeric(eval(call, envir = cpp_env)))
    }

    stop("the x supplied is neither a vector nor a matrix")

  })

}
