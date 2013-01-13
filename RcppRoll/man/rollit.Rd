\name{rollit}
\alias{rollit}
\title{Generate your own Weighted C++ Roll Function}
\usage{
  rollit(fun = "x", vector = FALSE, const_vars = NULL,
    combine = "+", final_trans = NULL, includes = NULL,
    depends = NULL, inline = TRUE, ...)
}
\arguments{
  \item{fun}{A character string defining the function call.
  The function must be in terms of variable \code{x}. The
  function will be applied individually to each element
  being 'roll'ed over, unless \code{vector=TRUE} is set.}

  \item{vector}{boolean; if \code{TRUE}, the function
  supplied attempts to take the entire vector passed, and
  returns a scalar result. Otherwise, we apply the function
  to each element of the vector \code{x} individually.}

  \item{const_vars}{Constant variables you would like to
  'live' within the sourced C++ function. Format is a named
  \code{list}; eg, you could pass \code{list(pi=pi)} to
  have \code{pi} as a constant variable available in the
  function you're calling. You can also supply any constant
  scalar-valued function of a vector; eg.
  \code{list(n="x.size()")} will assign \code{n} to the
  length of the sub-vector \code{x}.}

  \item{combine}{character; must be one of \code{"+", "-",
  "*", "/"}. how should we combine elements of the vector
  we are collapsing over?}

  \item{final_trans}{A final transformation to perform
  after 'rolling' over each element in the vector
  \code{x}.}

  \item{includes}{Other C++ libraries to include. For
  example, to include \code{boost/math.hpp}, you would pass
  \code{c("boost/math.hpp")}.}

  \item{depends}{Other libraries to link to. Linking is
  done through Rcpp attributes.}

  \item{inline}{boolean; mark the function generated as
  \code{inline}? This may or may not increase execution
  speed.}

  \item{...}{Additional arguments passed to
  \code{sourceCpp}.}
}
\value{
  A wrapper \R function to compiled C++ files, as generated
  through \code{sourceCpp}. The function accepts arguments
  for: a vector/matrix \code{x}, a window-size \code{n},
  the logical \code{by.column}, a vector of \code{weights},
  and a \code{by} statement. Note that the \code{weights}
  provides are un-normalized. By is used to 'skip' entries;
  so that setting \code{by=2} and a window size \code{n} of
  3 would effectively call the function on only the first
  and third elements of each vector passed through.
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
\note{
  All functions generated use Rcpp's \code{NumericVector}
  and \code{NumericMatrix} to interface to \R vectors and
  matrices. Because of this, any function that you would
  like to call on the whole vector being rolled over (eg,
  if you choose \code{vector=TRUE}) needs to be compatible
  with the Rcpp \code{NumericVector}. Elements within these
  vectors are translated as \code{double}s so any function
  that receives a \code{double} will work fine (eg, if you
  choose \code{vector=FALSE}).
}
\examples{
\dontrun{
x <- matrix(1:16, nrow=4)

## the squared rolling sum -- we square the sum of our rolled results
rolling_sqsum <- rollit( final_trans="x*x" )

rolling_sqsum( x, 4 )
rolling_sqsum( x, 4, by.column=FALSE )
cbind( as.vector(rolling_sqsum(x, 4)), apply(x, 2, function(x) { sum(x)^2 } ) )

## use the Rcpp sugar function 'mean' -- let's compute a 'sqrt mean'
rolling_mean <- rollit( "mean(x)", vector=TRUE, final_trans="sqrt( x )" )

## implement your own variance function
## we can use the sugar function 'mean' to get
## the mean of x

const_vars <- list(m = "mean(x)", n = "x.size()")
var_fun <- "( (x-m) * (x-m) )/(n-1)"
rolling_var <- rollit( var_fun, const_vars=const_vars )

x <- c(1, 5, 10, 15)
cbind( rolling_var(x, 2), roll_var(x, 2) )

## use a function from cmath

rolling_log10 <- rollit( "log10(x)" )
rolling_log10( 10^(1:5), 2 )

## rolling product
rolling_prod <- rollit( combine="*" )
rolling_prod( 1:10, 2 )

## a benchmark

if( require("microbenchmark") && require("zoo") ) {
  x <- rnorm(1E4)
  microbenchmark(
    rolling_var(x, 100),
    roll_var(x, 100),
    rollapply(x, 100, var),
    times=10
    )
  }}
}
\seealso{
  \code{\link{sourceCpp}} for information on how Rcpp
  compiles your functions, and
  \code{\link{get_rollit_source}} for inspection of the
  generated C++ code.
}

