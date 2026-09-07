# Run from the package root with identically compiled RcppRoll shared libraries:
# Rscript internal/benchmarks/correctness-performance.R base.so prior.so after.so output.csv
# Each library must be named RcppRoll.so (or .dll) in a separate directory.
# All three use the current public wrappers, so validation overhead is shared.
args <- commandArgs(TRUE)
stopifnot(length(args) == 4L)
load_api <- function(path) {
  dll <- dyn.load(normalizePath(path))
  api <- new.env(parent = globalenv())
  sys.source("R/RcppRoll.R", api)
  routines <- getDLLRegisteredRoutines(dll)$.Call
  for (name in names(routines))
    assign(paste0("C_", name), routines[[name]], api)
  api
}
apis <- setNames(lapply(args[1:3], load_api), c("base", "prior", "after"))
options(RcppRoll.threads = 1L)
x <- 1 + sin(seq_len(100000)) * 1e-4
exceptional <- x
exceptional[seq(1, length(x), by = 100)] <- 0
cases <- list(
  product_1000 = list(op = "roll_prod", x = x, n = 1000L),
  product_10000 = list(op = "roll_prod", x = x, n = 10000L),
  product_zeros = list(op = "roll_prod", x = exceptional, n = 1000L),
  variance_10 = list(op = "roll_var", x = x, n = 10L)
)
time_ms <- function(f) {
  f()
  reps <- 1L
  repeat {
    elapsed <- system.time(for (i in seq_len(reps)) f())[["elapsed"]]
    if (elapsed >= 0.03) break
    reps <- reps * 2L
  }
  median(replicate(5L,
    system.time(for (i in seq_len(reps)) f())[["elapsed"]] / reps)) * 1000
}
rows <- lapply(names(cases), function(name) {
  case <- cases[[name]]
  # The correctness fix may legitimately differ from the base on exceptional
  # inputs. Compare the final result to the already-correct direct PR kernel.
  expected <- apis$prior[[case$op]](case$x, case$n)
  actual <- apis$after[[case$op]](case$x, case$n)
  stopifnot(isTRUE(all.equal(actual, expected)))
  times <- vapply(apis, function(api) {
    f <- api[[case$op]]
    time_ms(function() f(case$x, case$n))
  }, numeric(1))
  data.frame(case = name, base_ms = times[1], prior_ms = times[2],
             after_ms = times[3], row.names = NULL)
})
result <- do.call(rbind, rows)
write.csv(result, args[4], row.names = FALSE)
print(result, row.names = FALSE)
