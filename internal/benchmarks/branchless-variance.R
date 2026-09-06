# Generate an experimental branchless variant from the pre-change source.
# This is an experiment, not part of the package implementation.
# Rscript --vanilla internal/benchmarks/branchless-variance.R before.cpp after.cpp [mask]
args <- commandArgs(TRUE)
stopifnot(length(args) %in% c(2L, 3L))
source <- paste(readLines(args[1]), collapse = "\n")
replace <- function(before, after) {
  stopifnot(length(gregexpr(before, source, fixed = TRUE)[[1]]) == 1L,
            grepl(before, source, fixed = TRUE))
  source <<- sub(before, after, source, fixed = TRUE)
}
replace("    if (is_nan(value)) {
      has_na = true;
    } else {
      total += value;
      ++count;
    }", "    bool ok = !is_nan(value);
    has_na |= !ok;
    total += ok ? value : -0.0;
    count += ok;")
replace("    if (is_nan(value)) {
      has_na = true;
    } else {
      weights_sum += weights[i];
      weighted_total += weights[i] * value;
      ++count;
    }", "    bool ok = !is_nan(value);
    has_na |= !ok;
    weights_sum += ok ? weights[i] : -0.0;
    weighted_total += ok ? weights[i] * value : -0.0;
    count += ok;")
replace("    if (!is_nan(value)) {
      double difference = value - mean;
      squares += difference * difference;
      residual += difference;
    }", "    bool ok = !is_nan(value);
    double difference = value - mean;
    squares += ok ? difference * difference : -0.0;
    residual += ok ? difference : -0.0;")
replace("    if (!is_nan(value)) {
      double difference = value - mean;
      squares += weights[i] * difference * difference;
      residual += weights[i] * difference;
    }", "    bool ok = !is_nan(value);
    double difference = value - mean;
    squares += ok ? weights[i] * difference * difference : -0.0;
    residual += ok ? weights[i] * difference : -0.0;")
if (length(args) == 3L) {
  # A source-level conditional can still compile to a branch. Explicit bit
  # masking tests genuinely branchless selection without fast-math or aliasing.
  source <- sub("#include <cmath>", "#include <cmath>\n#include <cstdint>", source,
                fixed = TRUE)
  source <- sub("inline double window_var", "inline double mask_missing(double value, bool ok) {
  std::uint64_t bits;
  std::memcpy(&bits, &value, sizeof(bits));
  std::uint64_t mask = 0 - static_cast<std::uint64_t>(ok);
  bits = (bits & mask) | (UINT64_C(0x8000000000000000) & ~mask);
  std::memcpy(&value, &bits, sizeof(value));
  return value;
}

inline double window_var", source, fixed = TRUE)
  window_start <- regexpr("inline double window_var", source, fixed = TRUE)[1L]
  prefix <- substr(source, 1L, window_start - 1L)
  variance_source <- substring(source, window_start)
  for (expr in c("value", "weights[i]", "weights[i] * value",
                 "difference * difference", "difference",
                 "weights[i] * difference * difference", "weights[i] * difference"))
    variance_source <- gsub(paste0("ok ? ", expr, " : -0.0"),
      paste0("mask_missing(", expr, ", ok)"), variance_source, fixed = TRUE)
  source <- paste0(prefix, variance_source)
}
writeLines(source, args[2])
