#' Rolling Sd
#'
#' This function implements a rolling sd with C++/Rcpp.
#' @param x An \R object of form: numeric vector, numeric matrix.
#' @param n integer; the window / subset size to roll over.
#' @param by.column boolean; if \code{TRUE} we loop over columns, otherwise we loop over rows.
#' @param weights A numeric vector of weights of same length as \code{n}.
#' @param normalize boolean; if \code{TRUE}, we normalize the weight of vectors supplied.
#' @note Elements in \code{weights} equal to 0 are skipped.
#' @export
roll_sd <- function( x, n, by.column=TRUE, weights=rep(1,n), normalize=FALSE ) {
	
	stopifnot( length(weights) == n )
	if( normalize ) {
		weights <- weights * length(weights) / sum(weights)
	}
	
	if( is.matrix(x) ) {
		if( by.column ) {
			stopifnot( n <= nrow(x) )
			return( .Call( "RcppRoll_roll_sd_numeric_matrix", x, as.integer(n), as.numeric(weights), PACKAGE="RcppRoll" ) )
		} else {
			stopifnot( n <= ncol(x) )
			return( t( .Call( "RcppRoll_roll_sd_numeric_matrix", t(x), as.integer(n), as.numeric(weights), PACKAGE="RcppRoll" ) ) )
		}
	}
	
	if( is.vector(x) ) {
		if( n > length(x) ) {
			stop("n cannot be greater than length(x).")
		}
		return( as.numeric( .Call( "RcppRoll_roll_sd_numeric_vector", x, as.integer(n), as.numeric(weights), PACKAGE="RcppRoll" ) ) )
	}
	
	stop("the x supplied is neither a vector or a matrix")

}

