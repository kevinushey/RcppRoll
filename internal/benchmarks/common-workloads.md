# Large vectors with small weight vectors

These measurements prioritize ordinary rolling workloads over gains for a
few very wide windows. They compare the PR base (`46760a6`), the previous PR
revision (`e9ba1f0`), and the source in the same commit as this report. A gain
in one row does not compensate for a regression in another; the remaining
costs are reported separately below.

## Results and remaining costs

For one million values and short weights, the large slowdowns in the previous
PR are substantially reduced. Finite weighted extrema are faster than the
base; weighted mean, variance, and SD are near the base in most clean-input
cases. This does not establish that every common case is regression-free.

| Case | PR base (ms) | Previous PR (ms) | Current (ms) | Current / base |
| --- | ---: | ---: | ---: | ---: |
| Sum, 20 weights | 1.375 | 1.406 | 1.375 | 1.00x |
| Mean, 20 weights | 1.406 | 2.219 | 1.531 | 1.09x |
| Min, 20 weights | 3.313 | 10.781 | 2.125 | 0.64x |
| Max, 20 weights | 3.344 | 10.813 | 2.125 | 0.64x |
| Prod, 20 weights | 1.719 | 1.719 | 1.719 | 1.00x |
| Var, 20 weights | 10.750 | 64.125 | 11.250 | 1.05x |
| Sd, 20 weights | 11.750 | 65.625 | 12.000 | 1.02x |
| Median, 20 weights | 121.000 | 120.500 | 120.000 | 0.99x |
| Variance, 99 weights | 48.000 | 264.500 | 48.000 | 1.00x |
| Unweighted variance, width 20 | 7.500 | 12.750 | 7.750 | 1.03x |
| Mean, 10 million values, 20 weights | 14.000 | 22.000 | 15.000 | 1.07x |

The following serial rows remain more than 15% slower than the PR base.
They are listed individually, rather than averaged away with faster cases:

| Case | PR base (ms) | Previous PR (ms) | Current (ms) | Current / base |
| --- | ---: | ---: | ---: | ---: |
| `mean_missing_20_remove_TRUE` | 3.938 | 5.250 | 4.875 | 1.24x |
| `mean_missing_99_remove_TRUE` | 19.000 | 22.500 | 22.000 | 1.16x |
| `min_missing_20_remove_FALSE` | 3.375 | 10.906 | 4.375 | 1.30x |
| `min_missing_99_remove_FALSE` | 16.000 | 54.500 | 19.250 | 1.20x |
| `max_missing_20_remove_FALSE` | 3.375 | 11.031 | 4.437 | 1.31x |
| `max_missing_99_remove_FALSE` | 15.750 | 54.000 | 19.500 | 1.24x |
| `prod_unweighted_1000_100k` | 0.609 | 0.766 | 0.781 | 1.28x |
| `var_by1000_weighted_20` | 0.035 | 0.070 | 0.070 | 2.00x |

The principal remaining common costs are weighted means with NAs removed
and weighted extrema preserving missing-value identity. The unweighted
product control retains the existing numerical guard's overhead. The very
wide stride variance control retains the general calculation: its time is
unchanged from the previous PR, with only 1,000 requested outputs. These
remaining costs should take priority over further optimization of rare cases.

The original width-1,000 rolling-sum control remains included in the CSV.
Its dispatch and incremental algorithm were not changed by this work.

Moving eligibility checks into work chunks avoids a serial scan limiting
parallel throughput. Representative parallel measurements are:

| Case | PR base (ms) | Previous PR (ms) | Current (ms) | Current / base |
| --- | ---: | ---: | ---: | ---: |
| 2 threads: `min_weighted_5` | 0.555 | 1.828 | 0.375 | 0.68x |
| 2 threads: `var_weighted_5` | 2.063 | 11.625 | 2.312 | 1.12x |
| 2 threads: `var_weighted_20` | 5.625 | 34.375 | 5.937 | 1.06x |
| 2 threads: `min_missing_20_remove_FALSE` | 1.812 | 5.766 | 2.438 | 1.34x |
| 14 threads: `min_weighted_5` | 0.332 | 0.895 | 0.246 | 0.74x |
| 14 threads: `var_weighted_5` | 1.031 | 5.563 | 1.141 | 1.11x |
| 14 threads: `var_weighted_20` | 2.875 | 16.562 | 3.063 | 1.07x |
| 14 threads: `min_missing_20_remove_FALSE` | 0.836 | 2.000 | 1.031 | 1.23x |

All 126 benchmark comparisons (82 serial, 22 at two threads, and 22 at 14
threads) agree numerically with both comparison revisions at tolerance
`1e-8` for these inputs. The complete serial and OpenMP test suites pass,
including numerical and missing-value regressions. `R CMD check --as-cran
--no-manual` reports no errors or warnings, only the development-version
metadata NOTE. Neither the tests nor these measurements imply a universal
numerical accuracy or performance guarantee.

## What changed

- Variance and SD use the ordinary two-pass arithmetic after an input scan
  establishes conservative magnitude bounds. Nonzero data
  and weights must lie between `1e-50` and `1e50` in absolute magnitude;
  zeros and missing observations are allowed. Extreme inputs retain the
  existing scaled calculation.
- The ordinary variance calculation still corrects for its rounded center.
  If that correction would remove more than one quarter of the second
  moment, the window is recomputed with the general kernel, which scales
  weighted centers. The input bounds
  also make a comparison of cross-products safe, avoiding an extra division
  per output. Both kernels share the final variance/SD semantics.
- Weighted extrema classify inputs before processing each chunk's windows.
  Finite inputs need only comparisons; inputs whose only non-finite value is NA track a
  missing flag and return NA directly. Mixed NA/NaN and non-finite weights or
  observations retain the general product-aware calculation.
- Means finalize ordinary lanes together, keeping overflow recovery outside
  the normal division loop. Missing-value classification calls `R_IsNA` only
  for values already known to be NaN.

The eligibility checks run inside the existing work chunks, alongside their
window calculations. This avoids a serial scan limiting OpenMP throughput;
an exceptional value also only affects its own chunks. Unweighted variance
retains the existing incremental algorithm and its crossover rules.
Strides larger than the window retain the general calculation, avoiding a
scan of the unused gaps between windows.

The tests cover ordinary weighted results, both normalization and `na.rm`
modes, matrices, strides, fill, integer inputs, signed zeros, missing product
identity, extreme magnitudes, and poorly centered wide windows. The existing
correctness regressions remain in place.

## Method

Measured on an Apple M4 Pro with 14 physical cores, macOS arm64, R 4.6.1,
Apple Clang 21, and `-O2`. The serial comparison uses builds without OpenMP.
The parallel comparisons use all three revisions with the same libomp
configuration and explicitly select two or 14 threads. Builds, tests, and
benchmark processes run separately.

The primary inputs contain one million normally distributed values and
nonuniform positive weights of length 5, 20, 50, or 99. Defaults are
`normalize = TRUE`, `na.rm = FALSE`, and `by = 1`. Weighted products use
values near one. Additional cases cover 1% randomly placed NAs, unnormalized
weights, unweighted controls, ten million values, ten-column matrices, and
`by = 10` or `by = 1000`. The original 100,000-value, width-1,000 controls
are included too.

Each result is the median of six paired timing batches. Library order
rotates, all three versions use the same number of repetitions, and the
fastest version is calibrated to at least 30 ms per batch. Garbage collection
runs outside the timed batches. All libraries use the same public wrappers;
both wrappers and benchmark closures are explicitly byte-compiled to avoid
asymmetric R JIT overhead.

`common-workloads.R` checks current results against the previous revision
before each measurement at tolerance `1e-8`. The `base_matches` column records
agreement with the PR base separately, since that version lacks correctness
fixes. CSV files contain medians; the corresponding `-batches.csv` files retain
all individual timings. Timings vary with hardware, compiler, and code layout;
small percentage differences should not be treated as portable guarantees.

## Reproduction

Install the three revisions into separate libraries with matching compiler
settings, then run from the package root:

```sh
Rscript internal/benchmarks/common-workloads.R \
  /path/to/base/RcppRoll/libs/RcppRoll.so \
  /path/to/prior/RcppRoll/libs/RcppRoll.so \
  /path/to/current/RcppRoll/libs/RcppRoll.so \
  /tmp/common-workloads
```

Each DLL must retain the name `RcppRoll.so` (or `RcppRoll.dll`). Set
`RCPPROLL_BENCH_CASES` to a regular expression to select cases. For matching
OpenMP builds, `RCPPROLL_BENCH_THREADS` selects the thread count; it defaults
to one. The retained parallel runs select clean weighted sum, mean, min,
max, variance, and SD at widths 5, 20, and 99, plus min/max with 1% NAs at
widths 20 and 99 and `na.rm = FALSE`.

The exact filter for the parallel runs is:

```sh
export RCPPROLL_BENCH_CASES='^(sum|mean|min|max|var|sd)_weighted_(5|20|99)$|^(min|max)_missing_(20|99)_remove_FALSE$'
export RCPPROLL_BENCH_THREADS=14
```
