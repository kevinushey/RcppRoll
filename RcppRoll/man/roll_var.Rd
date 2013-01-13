\name{roll_var}
\alias{roll_var}
\title{Rolling var}
\usage{
  roll_var(x, n, by.column = TRUE)
}
\arguments{
  \item{x}{an \R object of form: numeric vector, numeric
  matrix}

  \item{n}{an integer; number of elements to 'roll' over.}

  \item{by.column}{boolean; if \code{TRUE} we loop over
  columns, otherwise we loop over rows.}
}
\description{
  This function implements a rolling Var with C++/Rcpp.
}

