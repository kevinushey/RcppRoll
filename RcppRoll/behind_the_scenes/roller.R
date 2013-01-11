source("./behind_the_scenes/roll_R_file.R")
source("./behind_the_scenes/roll_Rcpp_file.R")
source("./behind_the_scenes/roll_RcppArmadillo_file.R")

#' This function acts as a generator for any of the 'roll'ing functions
#' used in this package.
#' 
#' @param fun the function name, as exported from one of the \code{includes}.
#' @param includes header includes. by default we include \code{Rcpp.h}.
#' @param depends other C++ package dependencies.
#' @param types the acceptable types, in terms of the available Rcpp objects.
roller <- function( fun, 
                    includes=NULL, 
                    depends=NULL,
                    types=c("NumericVector", "NumericMatrix")
) {
  
  cat("Rolling C++ source file for", fun, "...\n")
  roll_Rcpp_file( fun, includes, depends, types )
  cat("Rolling R source file for", fun, "...\n")
  roll_R_file( fun, includes, types )
  cat("Done!")
  
}

roller.arma <- function( fun, 
                         includes="RcppArmadillo.h",
                         depends="RcppArmadillo",
                         types=c("NumericVector", "NumericMatrix") 
) {
  
  cat("Rolling C++ source file for", fun, "...\n")
  roll_RcppArmadillo_file( fun, includes, depends, types )
  cat("Rolling R source file for", fun, "...\n")
  roll_R_file( fun, includes, types )
  cat("Done!")
  
}

roller("mean")
roller("sum")
roller("var")
roller("sd")
roller("max")
roller("min")

roller.arma("median")
roller.arma("prod")
roller.arma("stddev")