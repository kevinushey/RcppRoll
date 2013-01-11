roll_R_file <- function( fun, includes, types ) {
  
  R_types <- Kmisc::Replace(
    types,
    c("NumericVector", "NumericMatrix", "CharacterVector", "CharacterMatrix"),
    c("numeric vector", "numeric matrix", "character vector", "character matrix")
    )
  
  outFile <- file.path( getwd(), "R", paste0("roll_", fun, ".R") )
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
  
  ## roxygen
  w("#' Rolling ", fun)
  w("#'")
  w("#' This function implements a rolling ", fun, " with C++/Rcpp.")
  w("#' @param x an \\R object of form: ", paste( R_types, collapse=", " ))
  w("#' @param n an integer; number of elements to 'roll' over.")
  w("#' @export")
  
  ## function call
  w("roll_", fun, " <- function( x, n ) {")
  w1()
    
  ## Numeric Matrix handler
  
  if( "NumericMatrix" %in% types ) {
    w1("if( is.matrix(x) ) {")
    
    ## bounds checking
    w2("if( n > nrow(x) ) {")
    w3("stop(\"n cannot be greater than nrow(x).\")")
    w2("}")
    
    w2("return( .Call( \"RcppRoll_roll_", fun, "_numeric_matrix\", x, as.integer(n), PACKAGE=\"RcppRoll\" ) )")
    
    w1("}")
    w1("")
  }
  
  ## Character Matrix handler
  
  if( "CharacterMatrix" %in% types ) {
    w1("if( is.matrix(x) ) {")
    
    ## bounds checking
    w2("if( n > nrow(x) ) {")
    w3("stop(\"n cannot be greater than nrow(x).\")")
    w2("}")
    
    w2("return( .Call( \"RcppRoll_roll_", fun, "_character_matrix\", x, as.integer(n), PACKAGE=\"RcppRoll\" ) )")
    
    w1("}")
    w1("")
  }
  
  ## Numeric Vector handler
  
  if( "NumericVector" %in% types ) {
    w1("if( is.vector(x) ) {")
    
    ## bounds checking
    w2("if( n > length(x) ) {")
    w3("stop(\"n cannot be greater than length(x).\")")
    w2("}")
    
    w2("return( as.numeric( .Call( \"RcppRoll_roll_", fun, "_numeric_vector\", x, as.integer(n), PACKAGE=\"RcppRoll\" ) ) )")
    
    w1("}")
    w1("")
  }
  
  ## Character Vector handler
  
  if( "CharacterVector" %in% types ) {
    w1("if( is.vector(x) ) {")
    
    ## bounds checking
    w2("if( n > length(x) ) {")
    w3("stop(\"n cannot be greater than length(x).\")")
    w2("}")
    
    w2("return( .Call( \"RcppRoll_roll_", fun, "_character_vector\", x, as.integer(n), PACKAGE=\"RcppRoll\" ) )")
    
    w1("}")
    w1("")
  }
  
  ## if we get here, nothing matched
  w("stop(\"the x supplied is neither a vector or a matrix\")")
  w()
  w("}")
  w()
  
  close(conn)
  
}