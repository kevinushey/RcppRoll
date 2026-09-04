
# RcppRoll (development version)

## New features

- The rolling window computations are now parallelized with OpenMP, where
  support for it is available. By default, the number of threads is chosen
  by the OpenMP runtime (e.g. via `OMP_NUM_THREADS`); it can be set
  explicitly with `options(RcppRoll.threads = <n>)`, and
  `options(RcppRoll.threads = 1)` disables parallelization. Work is split
  into chunks whose boundaries do not depend on the thread count, so
  results are identical whatever the number of threads -- including on
  builds without OpenMP support at all.

- The new `roll_threads()` function reports the number of threads in use,
  or NA when the package was compiled without OpenMP support. The package
  reports this on attach as well; suppress the startup message with
  `options(RcppRoll.quiet = TRUE)`. Instructions for enabling OpenMP when
  installing from sources on macOS are in the README.

- The `partial` argument is now implemented. With `partial = TRUE`, windows at
  the edges of `x` are computed over however many elements are in range rather
  than filled, so the result has one element per element of `x`. This matches
  `zoo::rollapply(partial = TRUE)`. (#18)

  `partial = TRUE` cannot be combined with `weights`, and only `TRUE` or
  `FALSE` are accepted -- zoo's numeric "minimum observations" form is not
  supported. `fill` does not apply, and is warned about if supplied.

## Performance improvements

- Rolling windows are now computed incrementally where the operation admits
  it: a window carries its state forward and pays only for the observations
  that enter and leave, so the per-point cost no longer grows with the
  window size. Compensated summation and a rebuild-whenever-degraded check
  keep the sliding totals accurate -- on badly conditioned inputs the worst
  relative error falls from about 2e-2 to 3e-14 -- and running totals that
  overflow to infinity recover once the offending values leave the window.
  (#51)

- `roll_median()` now keeps large windows in a pair of heaps meeting at the
  median, so that sliding costs O(log n) per point rather than O(n). Windows
  below about two hundred observations keep the sorted-window representation,
  which remains faster there.

- `roll_prod()` now slides its window incrementally rather than recomputing
  each window in full, so its cost no longer grows with the window size. The
  window is carried as two stacks of partial products -- departing values are
  never divided out, so zeros, infinities and rounding behave as a fresh
  multiplication would.

- Matrices whose columns are too short to split into chunks are now
  parallelized across their columns instead, so wide matrices benefit from
  OpenMP too. As before, results are identical whatever the number of
  threads.

- The window loops that dominate small-window calls have been rewritten
  branchlessly where measurement showed a win, with bit-identical results:
  `roll_sum()` and `roll_mean()` with `na.rm = TRUE` run 1.4-2.5x faster,
  `roll_max()` with `na.rm = TRUE` 1.4-1.8x faster, and the weighted
  `roll_median()` about 1.8x faster.

## Bug fixes

- `roll_var()` and `roll_sd()` now compute a weighted variance when `weights`
  is supplied, rather than the variance of the weighted values. Weights are
  treated as frequency weights, so an equal weight vector gives the same
  answer as the unweighted routines. (#47)

- `roll_var()` and `roll_sd()` now keep each weight paired with its own value
  when `na.rm = TRUE`, instead of shifting the weights when NAs are dropped.
  (#47)

- `roll_var()` and `roll_sd()` now return NA for a window holding fewer than
  two non-missing values, matching `var()`. Previously an all-NA window gave
  0 and a single-value window gave NaN. (#47)

- Fixed an issue where `roll_mean()` produced incorrect results when both
  `weights` and `na.rm = TRUE` were used. The weights are now re-normalized
  after removing NAs. (#23)

- Fixed an issue where the weighted version of `roll_median()` ignored
  `na.rm`, and associated each weight with the sorted position of a value
  rather than with the value itself.

- Fixed an issue where rolling over a matrix with `weights` sized the output
  from `n` rather than from the weights, so each column read past its end
  and returned undefined values; and an issue where a matrix result with
  `by` greater than one was sized as though `by` were one, leaving rows that
  were never computed. (#51)

- Degenerate window geometries are now handled safely: calls with fewer
  observations than one window return an empty result rather than reading
  out of bounds (or failing with an opaque error), a window size of zero is
  rejected rather than corrupting memory, and arrays with three or more
  dimensions are rejected rather than rolled over their flattened data.

- The `roll_*()` functions now warn when `n` and `weights` are both supplied
  and disagree, since `weights` silently determines the window size. (#39)


# RcppRoll 0.3.2

- Resolve NOTE on CRAN.

# RcppRoll 0.3.1

- Fixed an issue where `roll_median()` produced incorrect results in the
  presence of NAs. (#42)

# RcppRoll 0.3.0

- Properly document the `align` argument -- the function accepts
  "center" rather than "middle". (#28)

- Fixed an issue where empty fills were not handled correctly.

- The interface has now been standardized such that each implemented window
  function has version center-aligned by default (e.g. `roll_mean()`), a
  left-aligned version (`roll_meanl()`), and right-aligned version
  (`roll_meanr()`).

- Implement rolling window functions for `mean()`, `median()`, `min()`,
  `max()`, `prod()`, `sum()`, `sd()` and `var()`.
