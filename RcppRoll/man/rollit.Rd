\name{rollit}
\alias{rollit}
\title{Generate your own C++ Roll Function}
\usage{
  rollit(name, fun = "x", combine = "+",
    final_trans = NULL, const_vars = NULL, includes = NULL,
    depends = NULL, ...)
}
\arguments{
  \item{name}{a name to use for the function.}

  \item{fun}{the function call. must be in terms of
  variable \code{x}.}

  \item{combine}{character; must be one of \code{"+", "-",
  "*", "/"}. how should we combine elements of the vector
  we are collapsing over?}

  \item{final_trans}{a final transformation to perform
  after 'rolling' over each element in the vector
  \code{x}.}

  \item{const_vars}{constant variables you would like to
  'live' within the sourced C++ function. format is a named
  \code{list}; eg, you could pass \code{list(pi=pi)} to
  have \code{pi} as a constant variable referencable in the
  function you're calling.}

  \item{includes}{other C++ libraries to include. for
  example, to include \code{boost/math.hpp}, you would pass
  \code{c("boost/math.hpp")}.}

  \item{depends}{other libraries to link to; linking is
  done through Rcpp attributes.}

  \item{...}{additional arguments passed to
  \code{sourceCpp}.

  This is then wrapped through a more generic 'rolling' sum
  function.}
}
\value{
  A wrapper \R function of the same name as \code{name}.
}
\description{
  Using this interface, you can define a function that you
  would like to be called on each sub-vector you are
  rolling over. The generated code is exposed via
  \code{sourceCpp}.
}
\details{
  By default, we include \code{<Rcpp.h>} in each file;
  however, you can include your own libraries with the
  \code{includes} call.
}
\examples{
## implement your own variance function
## we can use the sugar function 'mean' to get
## the mean of x

const_vars <- list(m = "mean(x)", n = "x.size()")
var_fun <- "pow( (x - m), 2)/(n-1)"
rollit( "rolling_var", var_fun, const_vars=const_vars )

x <- c(1, 5, 10, 15)
cbind( rolling_var(x, 2), roll_var(x, 2) )

## use a function from cmath

rolling_ex <- rollit( "rolling_ex",
  "log10(x)"
  )

## your C++ rolled functions will be much faster than an R
## apply equivalent, but perhaps not as fast as one of the
## internal versions...

if( require("microbenchmark") && require("zoo") ) {
  x <- rnorm(1E4)
  microbenchmark(
    rolling_var(x, 100),
    roll_var(x, 100),
    rollapply(x, 100, var),
    times=10
    )
  }
}

