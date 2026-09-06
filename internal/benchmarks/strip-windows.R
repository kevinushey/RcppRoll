# Per-element cost of every operation, before and after, single-threaded over
# one million doubles. Run from the package root with two independently built
# shared libraries (see build.R):
#   Rscript --vanilla internal/benchmarks/strip-windows.R before.so after.so output.csv
# Each library must be named RcppRoll.so (or .dll), in separate directories, and
# compiled with the same toolchain and flags.
args <- commandArgs(TRUE)
stopifnot(length(args) == 3L)
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
options(RcppRoll.threads = 1L)

set.seed(1)
N <- 1e6
x <- rnorm(N)
xna <- x
xna[sample(N, N / 100)] <- NA
m <- matrix(x, nrow = 100L)

# best of three batches, each long enough to time reliably
time_ns <- function(f, target = 0.1) {
  f()
  reps <- 1L
  repeat {
    t <- system.time(for (i in seq_len(reps)) f())[["elapsed"]]
    if (t >= 0.03)
      break
    reps <- reps * 2L
  }
  reps <- max(1L, as.integer(ceiling(reps * target / t)))
  best <- Inf
  for (k in 1:3) {
    t <- system.time(for (i in seq_len(reps)) f())[["elapsed"]]
    best <- min(best, t / reps)
  }
  best * 1e9 / N
}

rows <- list()
bench <- function(group, label, op, ...) {
  ns <- vapply(apis, function(api) {
    f <- api[[paste0("roll_", op)]]
    time_ns(function() f(...))
  }, numeric(1))
  rows[[length(rows) + 1L]] <<- data.frame(group = group, op = op, label = label,
    before = ns[["before"]], after = ns[["after"]])
}

ops <- c("sum", "mean", "min", "max", "prod", "var", "sd", "median")
for (op in ops) {
  for (n in c(3L, 5L, 10L, 20L, 50L, 100L, 200L, 500L, 2000L)) {
    bench("plain", sprintf("n=%d", n), op, x, n)
    bench("na.rm", sprintf("n=%d", n), op, xna, n, na.rm = TRUE)
  }
  cat(op, "done\n")
  flush.console()
}
for (op in c("sum", "mean", "min", "max", "prod", "var", "median")) {
  for (n in c(5L, 20L, 50L, 200L)) {
    w <- runif(n, 0.5, 1.5)
    bench("weighted", sprintf("n=%d", n), op, x, weights = w)
  }
}
for (op in c("sum", "mean", "max", "var")) {
  bench("other", "n=20 by=3", op, x, 20L, by = 3L)
  bench("other", "n=20 partial", op, x, 20L, partial = TRUE)
  bench("other", "n=20 fill=NA", op, x, 20L, fill = NA)
  bench("other", "n=10 matrix 100x10000", op, m, 10L)
}

res <- do.call(rbind, rows)
res$speedup <- res$before / res$after
write.csv(res, args[3], row.names = FALSE)

fmt <- function(v) formatC(v, digits = 2, format = "f", width = 6)
for (g in c("plain", "na.rm")) {
  cat(sprintf("\n== %s: ns per element, before -> after (speedup) ==\n", g))
  sub <- res[res$group == g, ]
  cat(sprintf("%-8s", "n"), sprintf("%-22s", ops), "\n", sep = "")
  for (label in unique(sub$label)) {
    cat(sprintf("%-8s", sub("n=", "", label)))
    for (op in ops) {
      r <- sub[sub$label == label & sub$op == op, ]
      cat(sprintf("%s->%s (%4.1fx)  ", fmt(r$before), fmt(r$after), r$speedup))
    }
    cat("\n")
  }
}
cat("\n== weighted ==\n")
sub <- res[res$group == "weighted", ]
for (op in unique(sub$op)) {
  cat(sprintf("%-8s", op))
  for (label in unique(sub$label)) {
    r <- sub[sub$label == label & sub$op == op, ]
    cat(sprintf("%s: %s->%s (%4.1fx)  ", label, fmt(r$before), fmt(r$after), r$speedup))
  }
  cat("\n")
}
cat("\n== other ==\n")
sub <- res[res$group == "other", c("op", "label", "before", "after", "speedup")]
sub$before <- round(sub$before, 2)
sub$after <- round(sub$after, 2)
sub$speedup <- round(sub$speedup, 1)
print(sub, row.names = FALSE)
cat("\n", R.version.string, "\n", Sys.info()[["sysname"]], "\n")
