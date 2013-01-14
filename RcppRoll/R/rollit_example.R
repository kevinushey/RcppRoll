#' 'rollit' Output -- Example Function
#' 
#' This presents the function signature for the output of
#' \link{rollit}.
#' 
#' @param x A vector/matrix of numeric type.
#' @param n integer; the window / subset size to roll over.
#' @param by.column boolean; if \code{TRUE} we loop over columns,
#' otherwise we loop over rows.
#' @param weights a vector of length \code{n}; specify the (relative) weighting
#' to assign to each element in the window. Note that these weights
#' will be normalized.
#' @param normalize boolean; if \code{TRUE} we normalize the weights to
#' sum to 1.
#' @return This function does not return anything; it merely exists
#' as a skeleton to provide documentation for your own \code{rollit}
#' generated functions.
#' @seealso \code{\link{rollit}}
rollit_example <- function(x, n, by.column, weights, normalize=TRUE) {
  return( invisible(NULL) )
}