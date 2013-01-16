\name{roll_var}
\alias{roll_var}
\title{Rolling Var}
\usage{
  roll_var(x, n, by.column = TRUE, weights = rep(1, n),
    normalize = FALSE)
}
\arguments{
  \item{x}{An \R object of form: numeric vector, numeric
  matrix.}

  \item{n}{integer; the window / subset size to roll over.}

  \item{by.column}{boolean; if \code{TRUE} we loop over
  columns, otherwise we loop over rows.}

  \item{weights}{A numeric vector of weights of same length
  as \code{n}.}

  \item{normalize}{boolean; if \code{TRUE}, we normalize
  the weight of vectors supplied.}
}
\description{
  This function implements a rolling var with C++/Rcpp.
}
\note{
  Elements in \code{weights} equal to 0 are skipped.
}

