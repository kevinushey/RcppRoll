
# RcppRoll 0.3.1  (UNRELEASED)

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

