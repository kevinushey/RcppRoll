##' @rdname RcppRoll
##' @export
roll_mean <- function(x, n, by.column = TRUE, weights = rep(1,n), normalize=FALSE) {
  return(.mean(x, n, by.column, weights, normalize))
}

##' @rdname RcppRoll
##' @export
roll_median <- function(x, n, by.column = TRUE, weights = rep(1,n), normalize=FALSE) {
  return(.median(x, n, by.column, weights, normalize))
}

##' @rdname RcppRoll
##' @export
roll_min <- function(x, n, by.column = TRUE, weights = rep(1,n), normalize=FALSE) {
  return(.min(x, n, by.column, weights, normalize))
}

##' @rdname RcppRoll
##' @export
roll_max <- function(x, n, by.column = TRUE, weights = rep(1,n), normalize=FALSE) {
  return(.max(x, n, by.column, weights, normalize))
}

##' @rdname RcppRoll
##' @export
roll_prod <- function(x, n, by.column = TRUE, weights = rep(1,n), normalize=FALSE) {
  return(.prod(x, n, by.column, weights, normalize))
}

##' @rdname RcppRoll
##' @export
roll_sum <- function(x, n, by.column = TRUE, weights = rep(1,n), normalize=FALSE) {
  return(.sum(x, n, by.column, weights, normalize))
}

##' @rdname RcppRoll
##' @export
roll_sd <- function(x, n, by.column = TRUE, weights = rep(1,n), normalize=FALSE) {
  return(.sd(x, n, by.column, weights, normalize))
}

##' @rdname RcppRoll
##' @export
roll_var <- function(x, n, by.column = TRUE, weights = rep(1,n), normalize=FALSE) {
  return(.var(x, n, by.column, weights, normalize))
}
