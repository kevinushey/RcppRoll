# Optimization measurements, 2026-09-05

Two changes were retained: early exits in direct variance/SD calculations
when `na.rm = FALSE`, and direct median selection for at most four output
windows per column. Buffer reservation and branchless variance experiments
were not retained.

## Method

- Baseline: `b82a8469e18f757336b6cc7d1c79b9ba90760330`.
- Windows, AMD Ryzen 9 5950X, R 4.5.1, Rtools45 GCC 14.2.0.
- Identical compilation flags for each pair: C++17, `-O2`, SSE2, and OpenMP
  where indicated. No fast-math or native-architecture flags.
- Public R wrappers, including argument validation, are included in timings.
  Input generation, correctness comparisons, warmup and explicit GC are outside
  the timed batches. Both DLLs run in the same R process.
- Seven batches per version and case, randomizing before/after order within
  each pair. Batch iteration counts are calibrated and shared between versions.
  Reported times are medians in milliseconds per call; raw batches are saved.
- Inputs use a fixed seed and 100,000 doubles. Small variance uses width 12;
  weighted variance uses 128 nonuniform positive weights. Periodic missing
  data has one NA every ten elements; sparse data has one every 1,000.
  Random-pattern cases independently insert NAs with the probability in the
  case name. The exact data and arguments are in `optimizations.R`.
- Four threads means `options(RcppRoll.threads = 4)`. A vector producing one
  or four outputs still runs serially; matrix cases exercise actual parallelism
  across columns. Results are specific to these inputs and this toolchain.

## Retained changes

Variance results below come from `variance-patterns.csv`: baseline versus the
variance change alone. Median results come from `dispatch.csv`: the variance
change versus variance plus median dispatch, with the buffer experiment removed.
Thus each retained change was measured independently.

| Case | One thread, before → after (ms) | Speedup | Four threads, before → after (ms) | Speedup |
|---|---:|---:|---:|---:|
| Small variance, periodic NAs | 1.790 → 0.400 | 4.47× | 0.887 → 0.306 | 2.90× |
| Weighted variance, periodic NAs | 16.517 → 0.330 | 50.04× | 7.257 → 0.286 | 25.39× |
| Weighted SD, periodic NAs | 16.147 → 0.353 | 45.72× | 7.361 → 0.295 | 24.99× |
| Weighted variance, random 1% NAs | 20.656 → 7.593 | 2.72× | 7.985 → 3.155 | 2.53× |
| Weighted variance, clean | 17.387 → 17.404 | 1.00× | 7.492 → 7.453 | 1.01× |
| Weighted variance, `na.rm = TRUE` | 16.595 → 16.666 | 1.00× | 7.116 → 7.156 | 0.99× |
| Median, one output from 100,000 values | 5.400 → 0.714 | 7.57× | 5.780 → 0.770 | 7.51× |
| Median, four outputs from 100,000 values | 5.414 → 2.841 | 1.91× | 5.773 → 3.015 | 1.91× |
| Median, one output in each of 100 columns | 5.371 → 1.235 | 4.35× | 1.530 → 0.398 | 3.84× |
| Median, four outputs in each of 100 columns | 5.249 → 3.460 | 1.52× | 1.552 → 1.014 | 1.53× |
| Median, ordinary width-1,000 rolling call | 10.806 → 10.802 | 1.00× | 4.461 → 4.538 | 0.98× |

The high missing-data speedups are deliberately workload-specific: with a
missing value every ten positions, early exits read at most ten observations
and skip the entire second pass. With very rare missing values, the small
variance case was effectively unchanged. Clean-data controls stayed within
about 2% in this run.

`noomp.csv` compares the original baseline with **both final changes**, compiled
without OpenMP. Small variance improved 4.44×, weighted variance with periodic
NAs 48.95×, the single-output median 8.03×, and the four-output median 2.11×.
The ordinary rolling median was unchanged. Clean weighted variance was 2.1%
slower in that run; this small difference is not evidence of a universal win.

## Early exits, branches and vectorization

The original variance loops were not SIMD-vectorized by this compiler under
the tested flags. GCC's `-fopt-info-vec-all` reports, for the baseline loops at
lines 1656, 1677, 1718 and 1740:

```text
missed: couldn't vectorize loop
missed: not vectorized: unsupported control flow in loop.
```

The assembly confirms scalar arithmetic (`addsd`, `mulsd`) and an existing
missing-value branch (`ucomisd` followed by `jp`). The early-exit version also
uses scalar loops. The `NA_RM` template parameter is a compile-time choice,
so `na.rm = TRUE` retains the original full two-pass calculation.

Two alternative full-pass implementations were also built and measured:

1. **Conditional expressions**, using `ok ? value : -0.0` and similar selects
   in both passes. GCC still emitted missing-value branches. This was slower
   than the original throughout these cases (`branchless.csv`); branchless
   source syntax did not yield branchless machine code.
2. **Explicit bit masking**, preserving the double representation through
   `memcpy`. This generated conditional moves (`cmovp`/`cmovnp`) instead of
   missing-value jumps in the inspected loops, but still no SIMD loop
   vectorization. Clean small variance was 1.63× slower and clean weighted
   variance 1.47× slower on one thread. With 50% random NAs, small variance
   improved 2.31× over the original, showing a case where avoiding unpredictable
   branches helps. Early exits improved that case 11.49× (`masked.csv` and
   `variance-patterns.csv`).

Thus branch predictability, work avoided, SIMD opportunities, memory traffic
and arithmetic dependencies all matter. Here an early exit avoids most of
the work and does not sacrifice existing vectorization. This conclusion does
not establish that branchless code is generally slower, or that other compilers
will make the same choices. The earlier sum/mean/min/max optimizations are
separate kernels and remain unchanged.

## Buffer experiment

`buffers.csv` compares variance-only with variance plus reserving product and
small-median buffers in the copied working accumulator, rather than the empty
prototype. Nine batches targeted 250 ms, using `proc.time()` in this initial
experiment (later experiments use `Sys.time()` for finer wall-clock timing).
Product cases ranged from unchanged to 6.5% faster. Median cases ranged from
3.8% slower to 9.5% faster, with no consistent improvement across execution
modes. The extra accumulator protocol was removed rather than retained on
that evidence. `buffers-experiment.patch` records the discarded implementation
against the variance-only version, before the median-dispatch change.

`variance.csv` is the initial, shorter variance-only experiment with the same
coarse timer. The expanded `variance-patterns.csv` is used for the main results.

## Correctness

The available testthat suite passed on the final build with OpenMP (697
expectations) and without OpenMP (696 expectations). Both had zero failures,
errors or test warnings. Eight tests were skipped in each run because `zoo`
was unavailable. New tests cover missing values at every position, NA versus
NaN results, infinities before missing values, median dispatch boundaries,
uniform weighted medians, matrices, fill, stride and partial alignment.
Every benchmark compares before/after output before timing.

## Reproduction

Run from the package root, using the baseline commit's `src` directory for
`baseline-src`, and the working tree's `src` directory for the final version:

```text
Rscript --vanilla internal/benchmarks/build.R baseline-src before
Rscript --vanilla internal/benchmarks/build.R src after
Rscript --vanilla internal/benchmarks/optimizations.R before/RcppRoll.dll after/RcppRoll.dll results.csv
```

Use `.so` in place of `.dll` on platforms that build shared libraries with that
extension. Pass `noomp` as the third argument to `build.R` for a serial build,
and set `BENCH_THREADS=1` for its benchmark. Build in new directories to avoid
stale objects. Keep R/toolchain/compiler flags identical across each comparison.
On the measured Windows host, compiler lookup required setting `R_MAKEVARS_USER`
to a temporary file containing the following; this changes executable lookup,
not optimization flags:

```make
BINPREF = C:/rtools45/x86_64-w64-mingw32.static.posix/bin/
export PATH := /c/rtools45/x86_64-w64-mingw32.static.posix/bin:/c/rtools45/usr/bin:$(PATH)
```

`BENCH_SECONDS`, `BENCH_TRIALS` and `BENCH_THREADS` control batch duration,
replicates and requested threads; an optional fourth benchmark argument filters
case names with a regular expression.

`branchless-variance.R before.cpp after.cpp` generates the conditional-expression
experiment from the original baseline. Add `mask` for the bit-mask experiment.
Compile the generated source alongside `init.c` with the same build helper.
Compiler diagnostics can be reproduced by compiling each source with the same
flags plus `-S -fverbose-asm -fopt-info-vec-all=vectorization.txt`.
