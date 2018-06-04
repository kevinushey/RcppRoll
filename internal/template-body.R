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
  result <- roll_%s_impl(
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
  colnames(result) <- colnames(x)
  result
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
  result <- roll_%s_impl(
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
  colnames(result) <- colnames(x)
  result
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
  result <- roll_%s_impl(
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
  colnames(result) <- colnames(x)
  result
}
