\name{roll_median}
\alias{roll_median}
\title{Rolling median}
\usage{
  roll_median(x, n, by.column = TRUE)
}
\arguments{
  \item{x}{an \R object of form: numeric vector, numeric
  matrix}

  \item{n}{an integer; number of elements to 'roll' over.}

  \item{by.column}{boolean; if \code{TRUE} we loop over
  columns, otherwise we loop over rows.}
}
\description{
  This function implements a rolling median with
  C++/RcppArmadillo.
}

