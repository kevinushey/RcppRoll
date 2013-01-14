\name{rollit_example}
\alias{rollit_example}
\title{'rollit' Output -- Example Function}
\usage{
  rollit_example(x, n, by.column, weights,
    normalize = TRUE)
}
\arguments{
  \item{x}{A vector/matrix of numeric type.}

  \item{n}{integer; the window / subset size to roll over.}

  \item{by.column}{boolean; if \code{TRUE} we loop over
  columns, otherwise we loop over rows.}

  \item{weights}{a vector of length \code{n}; specify the
  (relative) weighting to assign to each element in the
  window. Note that these weights will be normalized.}

  \item{normalize}{boolean; if \code{TRUE} we normalize the
  weights to sum to 1.}
}
\value{
  This function does not return anything; it merely exists
  as a skeleton to provide documentation for your own
  \code{rollit} generated functions.
}
\description{
  This presents the function signature for the output of
  \link{rollit}.
}
\seealso{
  \code{\link{rollit}}
}

