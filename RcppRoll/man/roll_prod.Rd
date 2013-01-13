\name{roll_prod}
\alias{roll_prod}
\title{Rolling prod}
\usage{
  roll_prod(x, n, by.column = TRUE)
}
\arguments{
  \item{x}{an \R object of form: numeric vector, numeric
  matrix}

  \item{n}{an integer; number of elements to 'roll' over.}

  \item{by.column}{boolean; if \code{TRUE} we loop over
  columns, otherwise we loop over rows.}
}
\description{
  This function implements a rolling Prod with
  C++/RcppArmadillo.
}

