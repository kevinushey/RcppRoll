\name{roll_min}
\alias{roll_min}
\title{Rolling Min}
\usage{
  roll_min(x, n, by.column = TRUE)
}
\arguments{
  \item{x}{an \R object of form: numeric vector, numeric
  matrix.}

  \item{n}{integer; the window / subset size to roll over.}

  \item{by.column}{boolean; if \code{TRUE} we loop over
  columns, otherwise we loop over rows.}
}
\description{
  This function implements a rolling min with C++/Rcpp.
}

