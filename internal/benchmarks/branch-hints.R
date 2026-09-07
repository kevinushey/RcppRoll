# Run from the package root with matching builds:
# Rscript internal/benchmarks/branch-hints.R before.so after.so output-prefix
# Exercise the hinted branches both with ordinary inputs and frequent fallbacks.
args <- commandArgs(TRUE)
stopifnot(length(args) == 3L)
load_api <- function(path) {
  dll <- dyn.load(normalizePath(path))
  api <- new.env(parent = globalenv())
  sys.source("R/RcppRoll.R", api)
  for (routine in getDLLRegisteredRoutines(dll)$.Call)
    assign(paste0("C_", routine$name), routine, api)
  for (name in ls(api))
    if (is.function(api[[name]])) api[[name]] <- compiler::cmpfun(api[[name]])
  api
}
apis <- setNames(lapply(args[1:2], load_api), c("before", "after"))
options(RcppRoll.threads = 1L)
set.seed(6120)
x <- rnorm(100000L)
w <- runif(20L, 0.5, 1.5)
tiny <- .Machine$double.xmin * .Machine$double.eps
cases <- list(
  mean_ordinary = list(op = "roll_mean", args = list(x = x, weights = w)),
  mean_all_missing = list(op = "roll_mean",
    args = list(x = rep(NA_real_, length(x)), weights = w)),
  mean_overflow = list(op = "roll_mean",
    args = list(x = rep(1e308, length(x)), weights = w)),
  mean_mixed_overflow = list(op = "roll_mean",
    args = list(x = c(x[1:50000], rep(1e308, 50000)), weights = w)),
  mean_underflow = list(op = "roll_mean",
    args = list(x = rep(c(NA_real_, 1e-30, 2e-30), 30000),
                weights = c(1, 1e-300, 2e-300), by = 3L, na.rm = TRUE)),
  normalization_ordinary = list(op = "roll_sum",
    args = list(x = rep(1, 10000), weights = rep(c(1, 2), 5000))),
  normalization_underflow = list(op = "roll_sum",
    args = list(x = rep(1, 10000), weights = rep(c(2, tiny), 5000)))
)
elapsed <- compiler::cmpfun(function(f, reps) {
  system.time(for (i in seq_len(reps)) f(), gcFirst = FALSE)[["elapsed"]]
})
rows <- raw <- list()
for (name in names(cases)) {
  case <- cases[[name]]
  calls <- lapply(apis, function(api) {
    f <- api[[case$op]]
    compiler::cmpfun(function() do.call(f, case$args))
  })
  before <- calls$before()
  after <- calls$after()
  stopifnot(identical(before, after))
  reps <- 1L
  repeat {
    gc()
    calibration <- vapply(calls, elapsed, numeric(1), reps = reps)
    if (min(calibration) >= 0.1) break
    reps <- reps * 2L
  }
  timings <- matrix(NA_real_, 9L, 2L, dimnames = list(NULL, names(calls)))
  for (batch in seq_len(nrow(timings))) {
    order <- if (batch %% 2L) c("before", "after") else c("after", "before")
    for (version in order) {
      gc()
      timings[batch, version] <- elapsed(calls[[version]], reps) * 1000 / reps
    }
  }
  medians <- apply(timings, 2L, median)
  rows[[name]] <- data.frame(case = name, before_ms = unname(medians["before"]),
                             after_ms = unname(medians["after"]),
                             after_over_before = unname(medians["after"] /
                                                          medians["before"]))
  raw[[name]] <- data.frame(case = name, batch = seq_len(nrow(timings)),
                            repetitions = reps, timings)
  print(rows[[name]], row.names = FALSE)
  write.csv(do.call(rbind, rows), paste0(args[3], ".csv"), row.names = FALSE)
  write.csv(do.call(rbind, raw), paste0(args[3], "-batches.csv"), row.names = FALSE)
}
