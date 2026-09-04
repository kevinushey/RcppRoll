
# RcppRoll 0.3.1  (UNRELEASED)

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

