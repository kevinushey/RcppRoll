# Branch-likelihood hint experiment

Explicit likelihood hints did not produce a repeatable improvement in ordinary
normalization or rolling means on this machine. Frequent fallback workloads
became 5–8% slower. The runtime hints were therefore not retained.

The baseline is PR revision `289618b`. The complete trial is preserved in
[branch-hints.patch](branch-hints.patch); it adds guarded `RCPPROLL_LIKELY`
and `RCPPROLL_UNLIKELY` wrappers and annotates two branches:

- The weight-normalization underflow recovery branch is unlikely.
- The ordinary-mean early return in `FinishWindows` is likely.

The patched source has SHA-256
`c382b7c8d8238af23dcd9f3351f7a0fd458e29fd3b8df6611cc8428545336bce`.
The production C++ source remains identical to `289618b`.

## Results

Representative final repeat measurements, milliseconds per call:

| Case | Without hints | With hints | Change |
| --- | ---: | ---: | ---: |
| Ordinary mean, 100,000 values | 0.173 | 0.172 | -0.6% |
| Ordinary normalization, 10,000 weights | 0.04077 | 0.04077 | 0.0% |
| Mean, all values missing | 0.414 | 0.438 | +5.7% |
| Mean, numerator underflow | 0.582 | 0.609 | +4.7% |
| Normalization with subnormal weights | 0.06689 | 0.07227 | +8.0% |

The first frequent-fallback run measured +6.7%, +5.4%, and +8.2% for the
last three rows, respectively. Overflow recovery was 1.8% faster initially
and 1.9% slower on repeat; mixed ordinary/overflow windows were unchanged
initially and 2.7% slower on repeat.

One initial common-workload result appeared promising: one million values,
99 weights, and 1% NAs retained was 7.6% faster. Two fresh timing processes
instead measured 26.0 to 26.5 ms (+1.9%) for that same case. At two and 14
threads it was 3.5% and 11.3% slower, respectively. Ordinary 20-weight means
were unchanged at both thread counts. These measurements do not support
keeping the hints, nor do they establish how other CPUs or compilers behave.

All 68 case runs, including repeats, passed numerical comparisons. The full
serial and OpenMP testthat suites also passed with the hints enabled. Both
the builtin and fallback macro definitions compiled in C++11 mode and passed
checks for true/false conversion, pointers, and single evaluation of an
expression with side effects. The feature-unavailable probe undefines
`__has_builtin` in a standalone translation unit to exercise the fallback.

## Portability

`__builtin_expect` is an intrinsic, so testing `#ifdef __builtin_expect` would
not detect support correctly. The wrappers first check whether `__has_builtin`
exists and then use `__has_builtin(__builtin_expect)`. Otherwise, both expand
to `!!(condition)`. This follows the feature-test mechanism documented by
[Clang](https://clang.llvm.org/docs/LanguageExtensions.html#builtin-expect)
and [GCC](https://gcc.gnu.org/onlinedocs/cpp/_005f_005fhas_005fbuiltin.html),
without requiring C++20 attributes or guessing support from compiler branding.

## Method and reproduction

Measured on an Apple M4 Pro, macOS arm64, R 4.6.1, and Apple Clang 21 at
`-O2`. Each comparison used matching serial or OpenMP builds. Builds and
tests completed before timing; benchmark processes ran sequentially.
Ordinary desktop services remained active.

The existing dispatch harness covered 13 normalization/mean controls. The
common-workload harness covered all 17 mean cases, two repeats of five
selected cases, and seven cases each at two and 14 threads. The common
harness loaded the same baseline library into both its `base` and `prior`
positions as an A/A timing control. It uses six rotating batches; dispatch
uses seven alternating batches, both calibrated to at least 30 ms.

[branch-hints.R](branch-hints.R) adds seven ordinary and frequent-fallback
controls. It requires bitwise-identical results and uses nine alternating
batches calibrated to at least 100 ms. These controls were run twice in
fresh processes. All individual timings are in
[branch-hints-batches.csv](branch-hints-batches.csv); medians and ratios are
in [branch-hints-results.csv](branch-hints-results.csv). `control_ms` is the
additional baseline measurement where the common harness was used.

To reproduce, create a dedicated worktree at `289618b`, build its native
library with `internal/benchmarks/build.R`, apply `branch-hints.patch`, and
build a second native library in another directory. Keep each library's name
`RcppRoll.so`. From the package root, run:

```sh
Rscript internal/benchmarks/branch-hints.R BEFORE AFTER output-fallbacks
RCPPROLL_BENCH_CASES='^mean_' Rscript internal/benchmarks/common-workloads.R BEFORE BEFORE AFTER output-means
RCPPROLL_BENCH_CASES='^(sum|mean|var)_weighted_.*normalize|^mean_rolling_width_1000$' Rscript internal/benchmarks/dispatch-performance.R BEFORE AFTER output-normalization
```

Replace `BEFORE` and `AFTER` with the shared-library paths. Copy this report's
benchmark script and patch into the baseline worktree before reproducing.
For the parallel comparisons, use matching OpenMP builds, set
`RCPPROLL_BENCH_THREADS` to `2` or `14`, and use this common-workload filter:

```sh
export RCPPROLL_BENCH_CASES='^mean_missing_(20|99)_remove_(TRUE|FALSE)$|^mean_weighted_(5|20|99)$'
```
