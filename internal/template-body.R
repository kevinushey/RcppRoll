#' @rdname RcppRoll-exports
#' @export
roll_%s <- function(x,
                    n = 1L,
                    weights = NULL,
                    by = 1L,
                    fill = numeric(0),
                    partial = FALSE,
                    align = c("center", "left", "right"),
                    normalize = TRUE,
                    na.rm = FALSE)
{
  n <- checkRollArgs(
    n, weights, by, partial, normalize, na.rm, missing(n), missing(fill))

  .Call(
    C_roll_%s_impl,
    x,
    as.integer(n),
    as.numeric(weights),
    as.integer(by),
    as.numeric(fill),
    as.logical(partial),
    as.character(match.arg(align)),
    as.logical(normalize),
    as.logical(na.rm)
  )
}

##' @rdname RcppRoll-exports
##' @export
roll_%sr <- function(x,
                     n = 1L,
                     weights = NULL,
                     by = 1L,
                     fill = NA,
                     partial = FALSE,
                     align = "right",
                     normalize = TRUE,
                     na.rm = FALSE)
{
  n <- checkRollArgs(
    n, weights, by, partial, normalize, na.rm, missing(n), missing(fill))

  .Call(
    C_roll_%s_impl,
    x,
    as.integer(n),
    as.numeric(weights),
    as.integer(by),
    as.numeric(fill),
    as.logical(partial),
    as.character(match.arg(align)),
    as.logical(normalize),
    as.logical(na.rm)
  )
}

##' @rdname RcppRoll-exports
##' @export
roll_%sl <- function(x,
                     n = 1L,
                     weights = NULL,
                     by = 1L,
                     fill = NA,
                     partial = FALSE,
                     align = "left",
                     normalize = TRUE,
                     na.rm = FALSE)
{
  n <- checkRollArgs(
    n, weights, by, partial, normalize, na.rm, missing(n), missing(fill))

  .Call(
    C_roll_%s_impl,
    x,
    as.integer(n),
    as.numeric(weights),
    as.integer(by),
    as.numeric(fill),
    as.logical(partial),
    as.character(match.arg(align)),
    as.logical(normalize),
    as.logical(na.rm)
  )
}
