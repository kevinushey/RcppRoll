#' Rolling Min
#'
#' This function implements a rolling min with C++/Rcpp.
#' @param x an \R object of form: numeric vector, numeric matrix.
#' @param n integer; the window / subset size to roll over.
#' @param by.column boolean; if \code{TRUE} we loop over columns, otherwise we loop over rows.
#' @export
roll_min <- function( x, n, by.column=TRUE ) {
	
	if( is.matrix(x) ) {
		if( n > nrow(x) ) {
			stop("n cannot be greater than nrow(x).")
		}
		return( .Call( "RcppRoll_roll_min_numeric_matrix", x, as.integer(n), as.logical(by.column), PACKAGE="RcppRoll" ) )
	}
	
	if( is.vector(x) ) {
		if( n > length(x) ) {
			stop("n cannot be greater than length(x).")
		}
		return( as.numeric( .Call( "RcppRoll_roll_min_numeric_vector", x, as.integer(n), PACKAGE="RcppRoll" ) ) )
	}
	
stop("the x supplied is neither a vector or a matrix")

}

