# Compare rolling-operation timings across installed RcppRoll versions.
#
# Install each version to compare into its own library, e.g.:
#   R CMD INSTALL --library=/tmp/lib-0.3.2 RcppRoll_0.3.2.tar.gz
# then run this script once per library:
#   Rscript internal/benchmark.R /tmp/lib-0.3.2 "0.3.2" /tmp/res-0.3.2.rds
# and compare the saved data frames. Requires the 'bench' package.

args <- commandArgs(trailingOnly = TRUE)
lib <- args[[1]]
label <- args[[2]]
out <- args[[3]]

library(RcppRoll, lib.loc = lib)

threads <- tryCatch(roll_threads(), error = function(e) 1L)
message(sprintf("== %s: RcppRoll %s, threads = %s",
                label, as.character(packageVersion("RcppRoll")), threads))

set.seed(1)
n <- 1e6
x <- rnorm(n)
xna <- x
xna[sample(n, n / 100)] <- NA
m <- matrix(rnorm(2000 * 200), 2000, 200)
w50 <- runif(50)

cases <- list(
  sum_w5          = quote(roll_sum(x, 5)),
  sum_w500        = quote(roll_sum(x, 500)),
  sum_narm_w50    = quote(roll_sum(xna, 50, na.rm = TRUE)),
  mean_w5         = quote(roll_mean(x, 5)),
  mean_w500       = quote(roll_mean(x, 500)),
  mean_narm_w50   = quote(roll_mean(xna, 50, na.rm = TRUE)),
  mean_wts_w50    = quote(roll_mean(x, weights = w50)),
  max_w50         = quote(roll_max(x, 50)),
  max_narm_w50    = quote(roll_max(xna, 50, na.rm = TRUE)),
  median_w25      = quote(roll_median(x, 25)),
  median_w500     = quote(roll_median(x, 500)),
  prod_w500       = quote(roll_prod(x, 500)),
  var_w50         = quote(roll_var(x, 50)),
  var_w500        = quote(roll_var(x, 500)),
  matrix_mean_w50 = quote(roll_mean(m, 50))
)

results <- lapply(names(cases), function(name) {
  bm <- bench::mark(
    eval(cases[[name]]),
    min_iterations = 7,
    check = FALSE
  )

  message(sprintf("%-16s %s", name, format(bm$median)))
  data.frame(
    build = label,
    case = name,
    median_s = as.numeric(bm$median),
    stringsAsFactors = FALSE
  )
})

results <- do.call(rbind, results)
results$threads <- threads
saveRDS(results, out)
