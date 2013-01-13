\name{roll_sd}
\alias{roll_sd}
\title{Rolling sd}
\usage{
  roll_sd(x, n, by.column = TRUE)
}
\arguments{
  \item{x}{an \R object of form: numeric vector, numeric
  matrix}

  \item{n}{an integer; number of elements to 'roll' over.}

  \item{by.column}{boolean; if \code{TRUE} we loop over
  columns, otherwise we loop over rows.}
}
\description{
  This function implements a rolling Sd with C++/Rcpp.
}

