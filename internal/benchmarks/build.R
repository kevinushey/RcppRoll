# Build an isolated native library from a directory containing RcppRoll.cpp
# and init.c, without modifying the package's src directory or installing it.
# Rscript --vanilla internal/benchmarks/build.R source-dir new-build-dir [noomp]
args <- commandArgs(TRUE)
stopifnot(length(args) %in% c(2L, 3L))
source_dir <- normalizePath(args[1], mustWork = TRUE)
dir.create(args[2], recursive = TRUE, showWarnings = FALSE)
build_dir <- normalizePath(args[2], mustWork = TRUE)
files <- c("RcppRoll.cpp", "init.c")
stopifnot(!any(file.exists(file.path(build_dir, files))))
stopifnot(all(file.copy(file.path(source_dir, files), build_dir)))
flags <- if (length(args) == 3L && args[3] == "noomp") character() else c(
  "PKG_CXXFLAGS = $(SHLIB_OPENMP_CXXFLAGS)",
  "PKG_LIBS = $(SHLIB_OPENMP_CXXFLAGS)")
writeLines(flags, file.path(build_dir, "Makevars"))
setwd(build_dir)
status <- system2(file.path(R.home("bin"), "R"), c("CMD", "SHLIB",
  "RcppRoll.cpp", "init.c", "-o", paste0("RcppRoll", .Platform$dynlib.ext)))
stopifnot(status == 0L)
