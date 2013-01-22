\name{roll_median}
\alias{roll_median}
\title{Rolling Median}
\usage{
  roll_median(x, n, by.column = TRUE)
}
\arguments{
  \item{x}{An \R object of form: numeric vector, numeric
  matrix.}

  \item{n}{integer; the window / subset size to roll over.}

  \item{by.column}{boolean; if \code{TRUE} we loop over
  columns, otherwise we loop over rows.}
}
\description{
  This function implements a rolling median with C++/Rcpp.
}

