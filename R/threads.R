#' Report the Number of Threads Used for Rolling Computations
#'
#' Reports how many threads the rolling computations would put to work on a
#' large enough input. The count honors \code{options(RcppRoll.threads)}
#' where that is set, and otherwise defers to the \code{OpenMP} runtime
#' default (typically controlled through the \code{OMP_NUM_THREADS}
#' environment variable). Small inputs are always computed serially, and
#' results do not depend on the number of threads used.
#'
#' @return An integer scalar: the maximum number of threads used, or
#'   \code{NA} if the package was compiled without \code{OpenMP} support --
#'   useful for checking what an installation from sources ended up with.
#'
#' @export
roll_threads <- function() {
  .Call(C_roll_threads_impl)
}
