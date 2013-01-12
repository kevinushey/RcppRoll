\name{roll_mean}
\alias{roll_mean}
\title{Rolling mean}
\usage{
  roll_mean(x, n, by.column = TRUE)
}
\arguments{
  \item{x}{an \R object of form: numeric vector, numeric
  matrix}

  \item{n}{an integer; number of elements to 'roll' over.}

  \item{by.column}{boolean; if \code{TRUE} we loop over
  columns, otherwise we loop over rows.}
}
\description{
  This function implements a rolling mean with C++/Rcpp.
}

