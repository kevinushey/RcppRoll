source("./behind_the_scenes/roll_R_file.R")

#' This function acts as a generator for any of the 'roll'ing functions
#' used in this package. Note that we only roll the accompanying \R files here.
#' 
#' @param fun the function name, as exported from one of the \code{includes}.
#' @param includes header includes. by default we include \code{Rcpp.h}.
#' @param depends other C++ package dependencies.
#' @param types the acceptable types, in terms of the available Rcpp objects.
#' @param by the program we give credit to for allowing us to compile.
roller <- function( fun, 
                    includes=NULL, 
                    depends=NULL,
                    types=c("NumericVector", "NumericMatrix"),
                    by="Rcpp"
) {
  
  ## cat("Rolling C++ source file for", fun, "...\n")
  ## roll_Rcpp_file( fun, includes, depends, types )
  cat("Rolling R source file for", fun, "...\n")
  roll_R_file( fun, includes, types, by )
  cat("Done!")
  
}

roller.arma <- function( fun, 
                         includes="RcppArmadillo.h",
                         depends="RcppArmadillo",
                         types=c("NumericVector", "NumericMatrix"),
                         by="RcppArmadillo"
) {
  
  ## cat("Rolling C++ source file for", fun, "...\n")
  ## roll_RcppArmadillo_file( fun, includes, depends, types )
  cat("Rolling R source file for", fun, "...\n")
  roll_R_file( fun, includes, types, by )
  cat("Done!")
  
}

# initialize
# outFile <- file.path( getwd(), "src", "source_files.cpp" )
# conn <- file( outFile, open='w' )
# cat( "#include <RcppArmadillo.h>\n", file=conn )
# cat( "using namespace Rcpp;\n\n", file=conn )
# close( conn )

roller("mean")
#roller("median")
roller("sum")
roller("var")
roller("sd")
roller("max")
roller("min")
roller("prod")

#roller.arma("median")
#roller.arma("prod")