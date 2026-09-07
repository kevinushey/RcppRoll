# Run from package root. All three libraries use the same public wrappers.
# Usage: Rscript internal/benchmarks/common-workloads.R base.so prior.so current.so output-prefix
# RCPPROLL_BENCH_THREADS selects a fixed OpenMP thread count (default: one).
args <- commandArgs(TRUE)
stopifnot(length(args) == 4L)
paths <- setNames(args[1:3], c("base", "prior", "current"))
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
apis <- lapply(paths, load_api)
threads <- as.integer(Sys.getenv("RCPPROLL_BENCH_THREADS", "1"))
stopifnot(!is.na(threads), threads > 0L)
options(RcppRoll.threads = threads)
set.seed(6100)
x <- rnorm(1000000L)
xna <- x
xna[sample.int(length(x), length(x) / 100L)] <- NA_real_
xprod <- 1 + x * 0.01
weight_sets <- setNames(lapply(c(5L, 20L, 50L, 99L), function(n) runif(n, 0.5, 1.5)), c("5", "20", "50", "99"))
cases <- list()
add_case <- function(case_name, operation, ...) {
  cases[[case_name]] <<- list(op = paste0("roll_", operation), args = list(...))
}
for (op in c("sum", "mean", "min", "max", "prod", "var", "sd", "median")) {
  for (n in c(5L, 20L, 50L, 99L)) {
    w <- weight_sets[[as.character(n)]]
    add_case(paste0(op, "_weighted_", n), op,
             x = if (op == "prod") xprod else x, weights = w)
  }
}
for (op in c("sum", "mean", "var")) {
  for (n in c(20L, 99L)) {
    w <- weight_sets[[as.character(n)]]
    if (op == "sum")
      add_case(paste0(op, "_na_rm_weighted_", n), op, x = xna,
               weights = w, na.rm = TRUE)
    add_case(paste0(op, "_raw_weighted_", n), op, x = x,
             weights = w, normalize = FALSE)
    add_case(paste0(op, "_unweighted_", n), op, x = x, n = n)
  }
}
large <- rep(x, 10L)
for (n in c(20L, 99L))
  add_case(paste0("mean_10million_weighted_", n), "mean", x = large,
           weights = weight_sets[[as.character(n)]])
for (op in c("mean", "min", "max", "var")) {
  for (n in c(20L, 99L)) {
    for (remove in c(FALSE, TRUE)) {
      add_case(paste0(op, "_missing_", n, "_remove_", remove), op,
               x = xna, weights = weight_sets[[as.character(n)]], na.rm = remove)
    }
  }
}
for (op in c("sum", "mean", "min", "max", "var", "sd")) {
  add_case(paste0(op, "_matrix_weighted_20"), op,
           x = matrix(x, ncol = 10L), weights = weight_sets[["20"]])
  add_case(paste0(op, "_by10_weighted_20"), op,
           x = x, weights = weight_sets[["20"]], by = 10L)
}
for (op in c("sum", "mean", "prod", "var"))
  add_case(paste0(op, "_unweighted_1000_100k"), op,
           x = 1 + sin(seq_len(100000L)) * 1e-4, n = 1000L)
for (op in c("min", "var"))
  add_case(paste0(op, "_by1000_weighted_20"), op,
           x = x, weights = weight_sets[["20"]], by = 1000L)
filter <- Sys.getenv("RCPPROLL_BENCH_CASES", "")
if (nzchar(filter)) cases <- cases[grepl(filter, names(cases))]
stopifnot(length(cases) > 0L)
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
  expected <- calls$prior()
  stopifnot(isTRUE(all.equal(expected, calls$current(), tolerance = 1e-8)))
  base_matches <- isTRUE(all.equal(expected, calls$base(), tolerance = 1e-8))
  gc()
  reps <- 1L
  repeat {
    calibration <- vapply(calls, elapsed, numeric(1), reps = reps)
    if (min(calibration) >= 0.03) break
    reps <- reps * 2L
  }
  timings <- matrix(NA_real_, 6L, 3L, dimnames = list(NULL, names(calls)))
  for (batch in seq_len(nrow(timings))) {
    order <- ((seq_along(calls) + batch - 2L) %% length(calls)) + 1L
    for (version in names(calls)[order]) {
      gc()
      timings[batch, version] <- elapsed(calls[[version]], reps) * 1000 / reps
    }
  }
  medians <- apply(timings, 2L, median)
  row <- data.frame(case = name, base_ms = unname(medians["base"]),
                    prior_ms = unname(medians["prior"]),
                    current_ms = unname(medians["current"]), base_matches = base_matches)
  rows[[name]] <- row
  raw[[name]] <- data.frame(case = name, batch = seq_len(nrow(timings)),
                            repetitions = reps, timings)
  print(row, row.names = FALSE)
  write.csv(do.call(rbind, rows), paste0(args[4], ".csv"), row.names = FALSE)
  write.csv(do.call(rbind, raw), paste0(args[4], "-batches.csv"), row.names = FALSE)
}
