\dontrun{
x <- matrix(1:16, nrow=4)

## the squared rolling sum -- we square the sum of our rolled results
rolling_sqsum <- rollit(final_trans = "x*x")
rolling_sqsum(x, 4)
cbind(as.vector(rolling_sqsum(x, 4)), apply(x, 2, function(x) { sum(x)^2 } ))

## implement your own variance function
## we can use the sugar function 'mean' to get
## the mean of x
const_vars <- list(m = "mean(x)")
var_fun <- "( (x-m) * (x-m) )/(N-1)"
rolling_var <- rollit(var_fun, const_vars = const_vars)
x <- c(1, 5, 10, 15)
cbind(rolling_var(x, 2), roll_var(x, 2))

## use a function from cmath
rolling_log10 <- rollit("log10(x)")
rolling_log10(10^(1:5), 2)

## rolling product
rolling_prod <- rollit(combine="*")
rolling_prod(1:10, 2)

## using weights to specify something like a 'by' argument
rolling_prod(1:10, 3, weights=c(1,0,1))

## a benchmark
if (require("microbenchmark") && require("zoo")) {
  x <- rnorm(1E6)
  microbenchmark(
    rolling_var(x, 100),
    roll_var(x, 100),
    rollapply(x, 100, var),
    times=1
  )
}
}
