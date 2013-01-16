roll_R_file <- function( fun, includes, types, by ) {
  
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
  
  .simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  
  ## roxygen
  w("#' Rolling ", .simpleCap(fun))
  w("#'")
  w("#' This function implements a rolling ", fun, " with C++/", by, ".")
  w("#' @param x An \\R object of form: ", paste( R_types, collapse=", " ), ".")
  w("#' @param n integer; the window / subset size to roll over.")
  w("#' @param by.column boolean; if \\code{TRUE} we loop over columns, otherwise we loop over rows.")
  w("#' @param weights A numeric vector of weights of same length as \\code{n}.")
  w("#' @param normalize boolean; if \\code{TRUE}, we normalize the weight of vectors supplied.")
  w("#' @note Elements in \\code{weights} equal to 0 are skipped.")
  w("#' @export")
  
  ## function call
  w("roll_", fun, " <- function( x, n, by.column=TRUE, weights=rep(1,n), normalize=FALSE ) {")
  w1()
  
  ## ensure weights correct
  w1("stopifnot( length(weights) == n )")
  
  ## normalize?
  w1("if( normalize ) {")
  w2("weights <- weights * length(weights) / sum(weights)")
  w1("}")
  w1()
  
  ## Numeric Matrix handler
  
  if( "NumericMatrix" %in% types ) {
    w1("if( is.matrix(x) ) {")
    
    ## bounds checking
    w2("if( by.column && n > nrow(x) ) {")
    w3("stop(\"n cannot be greater than nrow(x).\")")
    w2("} else if( !by.column && n > ncol(x) ) {")
    w3("stop(\"n cannot be greater than ncol(x).\")")
    w2("}")
    w2()
    
    w2("return( .Call( \"RcppRoll_roll_", fun, "_numeric_matrix\", x, as.integer(n), as.logical(by.column), as.numeric(weights), PACKAGE=\"RcppRoll\" ) )")
    
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
    
    w2("return( as.numeric( .Call( \"RcppRoll_roll_", fun, "_numeric_vector\", x, as.integer(n), as.numeric(weights), PACKAGE=\"RcppRoll\" ) ) )")
    
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