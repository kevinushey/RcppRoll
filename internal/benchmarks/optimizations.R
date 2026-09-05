# Run from the package root with two independently built DLLs/shared libraries:
# Rscript --vanilla internal/benchmarks/optimizations.R before after output.csv [filter]
# Each library must be named RcppRoll.dll (or RcppRoll.so), in separate directories.
# Compile both with the same R/toolchain/flags. The public R wrappers are included
# in timings; input creation, warmup, correctness checks and GC are excluded.
args <- commandArgs(TRUE)
stopifnot(length(args) >= 3L)
load_api <- function(path) {
  dll <- dyn.load(normalizePath(path))
  api <- new.env(parent = globalenv())
  sys.source("R/RcppRoll.R", api)
  routines <- getDLLRegisteredRoutines(dll)$.Call
  for (name in names(routines))
    assign(paste0("C_", name), routines[[name]], api)
  api
}
apis <- list(before = load_api(args[1]), after = load_api(args[2]))
set.seed(20260905)
x <- rnorm(100000)
missing_x <- x
missing_x[seq(3, length(x), 10)] <- NA_real_
sparse_x <- x
sparse_x[seq(3, length(x), 1000)] <- NA_real_
weights <- runif(128, 0.5, 1.5)
cases <- list()
add <- function(name, op, ...) {
  cases[[name]] <<- list(op = paste0("roll_", op), args = list(...))
}
add("variance-small-missing", "var", missing_x, 12L)
add("sd-small-missing", "sd", missing_x, 12L)
add("variance-weighted-missing", "var", missing_x, weights = weights)
add("sd-weighted-missing", "sd", missing_x, weights = weights)
add("variance-weighted-sparse", "var", sparse_x, weights = weights)
add("variance-small-clean", "var", x, 12L)
add("variance-weighted-clean", "var", x, weights = weights)
add("variance-weighted-remove", "var", missing_x, weights = weights, na.rm = TRUE)
for (p in c(0.001, 0.01, 0.1, 0.5)) {
  random_x <- x
  random_x[runif(length(x)) < p] <- NA_real_
  add(paste0("patterns-small-", p), "var", random_x, 12L)
  add(paste0("patterns-weighted-", p), "var", random_x, weights = weights)
}
add("buffers-prod-vector", "prod", 1 + x / 100, 64L)
add("buffers-prod-wide", "prod", matrix(1 + x / 100, nrow = 100), 64L)
add("buffers-median-vector", "median", x, 64L)
add("buffers-median-wide", "median", matrix(x, nrow = 100), 64L)
add("buffers-prod-partial", "prod", 1 + x / 100, 64L, partial = TRUE)
add("buffers-median-fill", "median", x, 64L, fill = NA_real_)
add("dispatch-median-one", "median", x, length(x))
add("dispatch-median-four", "median", x, length(x) - 3L)
add("dispatch-median-one-wide", "median", matrix(x, nrow = 1000), 1000L)
add("dispatch-median-four-wide", "median", matrix(x, nrow = 1000), 997L)
add("dispatch-median-small-one", "median", matrix(x, nrow = 100), 100L)
add("dispatch-median-small-four", "median", matrix(x, nrow = 100), 97L)
add("dispatch-median-fill", "median", x, length(x), fill = NA_real_)
add("dispatch-median-remove", "median", missing_x, length(x), na.rm = TRUE)
add("dispatch-median-uniform", "median", x, weights = rep(1, length(x)))
add("dispatch-median-normal", "median", x, 1000L)
if (length(args) >= 4L) cases <- cases[grepl(args[4], names(cases))]
stopifnot(length(cases) > 0L)
target <- as.numeric(Sys.getenv("BENCH_SECONDS", "0.12"))
trials <- as.integer(Sys.getenv("BENCH_TRIALS", "7"))
thread_counts <- as.integer(strsplit(Sys.getenv("BENCH_THREADS", "1,4"), ",")[[1]])
elapsed <- function(f, n) {
  start <- as.numeric(Sys.time())
  for (i in seq_len(n)) f()
  as.numeric(Sys.time()) - start
}
raw <- list()
for (threads in thread_counts) {
  options(RcppRoll.threads = threads)
  for (name in names(cases)) {
    case <- cases[[name]]
    calls <- lapply(apis, function(api) {
      f <- api[[case$op]]
      function() do.call(f, case$args)
    })
    stopifnot(isTRUE(all.equal(calls$before(), calls$after(), tolerance = 1e-12)))
    counts <- vapply(calls, function(f) {
      f()
      n <- 1L
      repeat {
        t <- elapsed(f, n)
        if (t >= 0.03) break
        n <- n * 2L
      }
      max(1L, as.integer(ceiling(n * target / t)))
    }, integer(1))
    # Keep paired batches equally sized without letting a large speedup turn
    # the slower version's batch into many seconds of work.
    counts[] <- min(max(counts), 4L * min(counts))
    for (trial in seq_len(trials)) {
      for (version in sample(names(calls))) {
        gc(FALSE)
        ms <- 1000 * elapsed(calls[[version]], counts[[version]]) / counts[[version]]
        raw[[length(raw) + 1L]] <- data.frame(case = name, threads = threads,
          trial = trial, version = version, iterations = counts[[version]], ms = ms)
      }
    }
    cat(name, "threads", threads, "done\n")
    flush.console()
  }
}
raw <- do.call(rbind, raw)
write.csv(raw, sub("[.]csv$", "-raw.csv", args[3]), row.names = FALSE)
medians <- aggregate(ms ~ case + threads + version, raw, median)
summary <- reshape(medians, idvar = c("case", "threads"), timevar = "version",
                   direction = "wide")
summary$speedup <- summary$ms.before / summary$ms.after
summary <- summary[order(summary$case, summary$threads), ]
write.csv(summary, args[3], row.names = FALSE)
print(summary, row.names = FALSE, digits = 4)
cat("\n", R.version.string, "\n", Sys.info()[["sysname"]], "\n")
