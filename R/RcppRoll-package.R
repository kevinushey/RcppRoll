#' RcppRoll
#'
#' This package implements a number of 'roll'-ing functions for \R
#' vectors and matrices.
#'
#' Currently, the exported functions are:
#'  \itemize{
#'  \item{\code{\link{roll_max}}}
#'  \item{\code{\link{roll_mean}}}
#'  \item{\code{\link{roll_median}}}
#'  \item{\code{\link{roll_min}}}
#'  \item{\code{\link{roll_prod}}}
#'  \item{\code{\link{roll_sd}}}
#'  \item{\code{\link{roll_sum}}}
#'  \item{\code{\link{roll_var}}}
#'  }
#'
#' @section Parallelization:
#'
#' When the package is compiled with \code{OpenMP} support, the rolling
#' window computations are parallelized across threads. By default, the
#' number of threads is chosen by the \code{OpenMP} runtime (typically
#' controlled through the \code{OMP_NUM_THREADS} environment variable);
#' it can be set explicitly with \code{options(RcppRoll.threads = <n>)},
#' and parallelization can be disabled with
#' \code{options(RcppRoll.threads = 1)}. Small inputs are always computed
#' serially, and results are identical whatever the number of threads --
#' including on builds without \code{OpenMP} support at all.
#'
#' Use \code{\link{roll_threads}()} to check how many threads are in use,
#' or whether the installed package has \code{OpenMP} support at all. The
#' package also reports this when attached; suppress that message with
#' \code{options(RcppRoll.quiet = TRUE)}.
#'
#' @name RcppRoll
#' @docType package
#' @useDynLib RcppRoll, .registration = TRUE, .fixes = "C_"
NULL

