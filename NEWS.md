
# RcppRoll 0.3.1  (UNRELEASED)

- The rolling window computations are now parallelized with OpenMP, where
  support for it is available. By default, the number of threads is chosen
  by the OpenMP runtime (e.g. via `OMP_NUM_THREADS`); it can be set
  explicitly with `options(RcppRoll.threads = <n>)`, and
  `options(RcppRoll.threads = 1)` disables parallelization. Work is split
  into chunks whose boundaries do not depend on the thread count, so
  results are identical whatever the number of threads -- including on
  builds without OpenMP support at all.

- The `partial` argument is now implemented. With `partial = TRUE`, windows at
  the edges of `x` are computed over however many elements are in range rather
  than filled, so the result has one element per element of `x`. This matches
  `zoo::rollapply(partial = TRUE)`. (#18)

  `partial = TRUE` cannot be combined with `weights`, and only `TRUE` or
  `FALSE` are accepted -- zoo's numeric "minimum observations" form is not
  supported. `fill` does not apply, and is warned about if supplied.

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

- The `roll_*()` functions now warn when `n` and `weights` are both supplied
  and disagree, since `weights` silently determines the window size. (#39)


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

