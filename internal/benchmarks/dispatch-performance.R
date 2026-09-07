# Run from the package root with identically compiled shared libraries:
# Rscript internal/benchmarks/dispatch-performance.R before.so after.so output-prefix
# Both libraries must be named RcppRoll.so (or .dll) in separate directories.
args <- commandArgs(TRUE)
stopifnot(length(args) == 3L)
load_api <- function(path) {
  dll <- dyn.load(normalizePath(path))
  api <- new.env(parent = globalenv())
  sys.source("R/RcppRoll.R", api)
  for (routine in getDLLRegisteredRoutines(dll)$.Call)
    assign(paste0("C_", routine$name), routine, api)
  # Explicit compilation avoids asymmetric JIT warm-up between identical
  # wrappers/closures; an A/A comparison exposed a ~2 us bias otherwise.
  for (name in ls(api)) {
    if (is.function(api[[name]])) api[[name]] <- compiler::cmpfun(api[[name]])
  }
  api
}
apis <- setNames(lapply(args[1:2], load_api), c("before", "after"))
options(RcppRoll.threads = 1L)
cases <- list()
add_case <- function(case_name, op, ...) {
  cases[[case_name]] <<- list(op = paste0("roll_", op), args = list(...))
}
for (op in c("sum", "mean", "var", "prod")) {
  for (outputs in c(1L, 2L, 4L, 5L, 8L, 9L, 12L, 16L, 17L)) {
    n <- 100000L
    x <- 1 + sin(seq_len(n + outputs - 1L)) * 1e-4
    add_case(paste0(op, "_outputs_", outputs), op, x = x, n = n)
  }
  for (outputs in c(1L, 4L, 8L, 16L)) {
    n <- 100000L
    x <- 1 + sin(seq_len(n + (outputs - 1L) * 3L)) * 1e-4
    x[seq(100L, length(x), by = 100L)] <- NA_real_
    add_case(paste0(op, "_na_stride3_outputs_", outputs), op,
             x = x, n = n, by = 3L, na.rm = TRUE)
  }
  for (outputs in c(1L, 4L, 16L)) {
    n <- 1000L
    x <- 1 + sin(seq_len(n + outputs - 1L)) * 1e-4
    add_case(paste0(op, "_matrix_outputs_", outputs), op,
             x = matrix(rep(x, 32L), ncol = 32L), n = n)
  }
  x <- 1 + sin(seq_len(100000L)) * 1e-4
  for (n in c(10L, 1000L))
    add_case(paste0(op, "_rolling_width_", n), op, x = x, n = n)
}
x <- 1 + sin(seq_len(100000L)) * 1e-4
add_case("sd_outputs_1", "sd", x = x, n = length(x))
x[seq(100L, length(x), by = 100L)] <- 0
add_case("prod_rolling_zeros", "prod", x = x, n = 1000L)
for (n in c(1000L, 1000000L)) {
  x <- 1 + sin(seq_len(n)) * 1e-4
  w <- 1 + cos(seq_len(n)) * 0.25
  for (op in c("sum", "mean", "var")) {
    for (normalize in c(FALSE, TRUE))
      add_case(paste0(op, "_weighted_", n, "_normalize_", normalize), op,
               x = x, weights = w, normalize = normalize)
  }
}
filter <- Sys.getenv("RCPPROLL_BENCH_CASES", "")
if (nzchar(filter)) cases <- cases[grepl(filter, names(cases))]
stopifnot(length(cases) > 0L)
# Warm each callable and calibrate paired batches to at least 30 ms for the
# faster version. Alternate execution order to reduce thermal/order bias.
elapsed <- function(f, reps) {
  system.time(for (i in seq_len(reps)) f(), gcFirst = FALSE)[["elapsed"]]
}
raw <- list()
summary <- list()
for (name in names(cases)) {
  case <- cases[[name]]
  calls <- lapply(apis, function(api) {
    f <- api[[case$op]]
    compiler::cmpfun(function() do.call(f, case$args))
  })
  stopifnot(isTRUE(all.equal(calls$before(), calls$after(), tolerance = 1e-8)))
  gc()
  reps <- 1L
  repeat {
    calibration <- vapply(calls, elapsed, numeric(1), reps = reps)
    if (min(calibration) >= 0.03) break
    reps <- reps * 2L
  }
  timings <- matrix(NA_real_, 7L, 2L, dimnames = list(NULL, names(calls)))
  for (batch in seq_len(nrow(timings))) {
    order <- if (batch %% 2L) c("before", "after") else c("after", "before")
    for (version in order) {
      gc()
      timings[batch, version] <- elapsed(calls[[version]], reps) * 1000 / reps
    }
  }
  medians <- apply(timings, 2L, median)
  row <- data.frame(case = name, before_ms = unname(medians["before"]),
                    after_ms = unname(medians["after"]),
                    speedup = unname(medians["before"] / medians["after"]))
  print(row, row.names = FALSE)
  summary[[name]] <- row
  raw[[name]] <- data.frame(case = name, batch = seq_len(nrow(timings)),
                            repetitions = reps, timings)
}
write.csv(do.call(rbind, summary), paste0(args[3], ".csv"), row.names = FALSE)
write.csv(do.call(rbind, raw), paste0(args[3], "-batches.csv"), row.names = FALSE)
