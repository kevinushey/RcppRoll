\name{roll_max}
\alias{roll_max}
\title{Rolling Max}
\usage{
  roll_max(x, n, by.column = TRUE)
}
\arguments{
  \item{x}{an \R object of form: numeric vector, numeric
  matrix.}

  \item{n}{integer; the window / subset size to roll over.}

  \item{by.column}{boolean; if \code{TRUE} we loop over
  columns, otherwise we loop over rows.}
}
\description{
  This function implements a rolling max with C++/Rcpp.
}

