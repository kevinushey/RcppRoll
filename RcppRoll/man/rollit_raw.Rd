\name{rollit_raw}
\alias{rollit_raw}
\title{Wrap in a Raw C++ Function}
\usage{
  rollit_raw(fun, depends = NULL, includes = NULL,
    inline = TRUE, ...)
}
\arguments{
  \item{fun}{A character string defining the function call.
  See examples for usage.}

  \item{includes}{Other C++ libraries to include. For
  example, to include \code{boost/math.hpp}, you would pass
  \code{c("boost/math.hpp")}.}

  \item{depends}{Other libraries to link to. Linking is
  done through Rcpp attributes.}

  \item{inline}{boolean; mark this function as inline? This
  may or may not increase execution speed.}

  \item{...}{Optional arguments passed to
  \code{sourceCpp}.}
}
\description{
  Using this, you can write and wrap in your own C++
  function. The onus is, however, on you to know what
  you're doing.
}
\details{
  The signature of the function is fixed as:

  \code{double <name>( NumericVector x, const NumericVector
  weights )}

  which are passed in from the generated \R function.
}
\examples{
\dontrun{
## implement a weighted rolling 'sum of squares'
fun <- "
double out = 0;
for( int i=0; i < x.size(); i++ ) {
  out += weights[i] * x[i] * x[i];
  }
return( out );
"
rolling_sumsq <- rollit_raw( fun )
x <- 1:5
rolling_sumsq( x, 5 ) == sum( x^2 )
}
}

