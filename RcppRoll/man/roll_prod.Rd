\name{roll_prod}
\alias{roll_prod}
\title{Rolling Prod}
\usage{
  roll_prod(x, n, by.column = TRUE)
}
\arguments{
  \item{x}{an \R object of form: numeric vector, numeric
  matrix.}

  \item{n}{integer; the window / subset size to roll over.}

  \item{by.column}{boolean; if \code{TRUE} we loop over
  columns, otherwise we loop over rows.}
}
\description{
  This function implements a rolling prod with
  C++/RcppArmadillo.
}

