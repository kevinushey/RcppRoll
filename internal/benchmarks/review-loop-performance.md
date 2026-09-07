# Performance check for the review-loop fixes

The subnormal-weight and weighted-mean fixes initially slowed normalization
of one million weights by 21–30%. Moving exponent recovery out of the ordinary
normalization loops removes most of that regression. Small costs remain; this
report does not claim unchanged performance for every workload.

The regression baseline is `75e2a1f`, the PR head before the review loop. The
initial fixed revision is `e2bb960`. Final measurements use the C++ source in
the same commit as this report, with SHA-256
`e2378c8eebc8774f94c879d4ce9a26ce3f8e321f2829631fab000d11349d2c1b`.
The common-workload and correctness harnesses also retain the original PR base,
`46760a6`, as a separate comparison. Existing costs relative to that base are
documented in [common-workloads.md](common-workloads.md).

## Normalization regression and fix

Initial measurements, milliseconds per call:

| One million normalized weights | Before review | Initial fixes | Change |
| --- | ---: | ---: | ---: |
| Sum | 3.000 | 3.875 | +29.2% |
| Mean | 2.938 | 3.812 | +29.8% |
| Variance | 4.875 | 5.875 | +20.5% |

The final implementation detects underflow while validating the weights,
retains the ordinary division/store loop, and performs exponent recovery only
when an intermediate ratio is subnormal. Its mean guard tests the numerator
before examining the surviving-weight denominator. Both numerical fixes and
their regression tests remain in place.

Final full-suite measurements:

| Case | Before review (ms) | Final (ms) | Change |
| --- | ---: | ---: | ---: |
| Sum, one million normalized weights | 3.062 | 3.188 | +4.1% |
| Mean, one million normalized weights | 3.125 | 3.250 | +4.0% |
| Variance, one million normalized weights | 5.125 | 5.375 | +4.9% |
| Sum, one million values, 20 weights | 1.406 | 1.406 | 0.0% |
| Mean, one million values, 20 weights | 1.594 | 1.594 | 0.0% |
| Variance, one million values, 20 weights | 11.500 | 11.500 | 0.0% |
| Mean with 1% NAs removed, 20 weights | 5.000 | 5.125 | +2.5% |
| Mean with 1% NAs removed, 99 weights | 23.000 | 22.750 | -1.1% |

Two additional paired runs put the million-weight sum at +4.0–4.1%, mean
at +6.1%, and variance at +2.4%. Those remaining normalization costs are
measured overhead, not dismissed as noise. The NA-removing mean at width 20
was unchanged and +1.2% in its two repeats.

## Coverage and outliers

All 220 final benchmark cases passed their numerical comparisons:

- All 82 serial cases in `common-workloads.R`.
- All 86 serial cases in `dispatch-performance.R`.
- All four cases in `correctness-performance.R`.
- 24 common-workload cases at two threads and the same 24 at 14 threads.
  These include the previously documented 22 parallel controls plus the two
  NA-removing weighted mean cases affected by the review fixes.

The original PR base also matched every final common-workload case at the
harness's `1e-8` tolerance. Serial and OpenMP testthat suites pass, including
the subnormal normalization and weighted-mean underflow regressions.

The largest full-run common-workload outlier was the unweighted mean over
100,000 values with width 1,000: 0.688 to 0.766 ms (+11.4%). Its two common
harness repeats were +6.7% and +3.3%. The same input in the dispatch harness
was unchanged initially, then +4.4% and +2.1% in repeats. This variability
does not establish a stable 11% regression, but it also does not establish
zero overhead. Its incremental algorithm was not changed.

Excluding that outlier, no serial common-workload row was more than 2.9%
slower in the full run. The largest measured slowdowns at two and 14 threads
were 2.6% and 4.8%, respectively. All four safeguard controls were unchanged
or faster. Full per-case results, including slower rows, are retained in
[review-loop-performance.csv](review-loop-performance.csv).

## Method and reproduction

Measured on an Apple M4 Pro with 14 physical cores, macOS arm64, R 4.6.1,
and Apple Clang 21 at `-O2`. Serial libraries were built with matching serial
flags; OpenMP libraries were built with matching libomp flags. Builds, tests,
and benchmark runs were performed separately. Benchmark processes did not
run concurrently with one another. Normal desktop services remained active.

The existing scripts, input seeds, byte-compiled wrappers, calibrated batch
lengths, and rotating/alternating library order were preserved. Common cases
use six paired batches; dispatch uses seven. Both calibrate the faster library
to at least 30 ms per batch. The safeguard harness uses its existing five-batch
median. All reported timings are milliseconds per call.

[review-loop-performance-batches.csv](review-loop-performance-batches.csv)
retains every paired batch. The safeguard script only emits its medians.
The `initial-normalization` rows refer to `e2bb960`; all `review-loop-*` rows
refer to the final source above. `repeat-*` rows are fresh timing processes
against the same binaries. Initial and final batches were not pooled.

Build each revision using `internal/benchmarks/build.R`, retaining the shared
library name `RcppRoll.so` in separate directories. From the package root:

```sh
Rscript internal/benchmarks/common-workloads.R BASE PRIOR FINAL output-common
Rscript internal/benchmarks/dispatch-performance.R PRIOR FINAL output-dispatch
Rscript internal/benchmarks/correctness-performance.R BASE PRIOR FINAL output-correctness.csv
```

Replace `BASE`, `PRIOR`, and `FINAL` with the corresponding shared-library
paths. Use serial builds for the complete runs above. For each parallel run,
use matching OpenMP builds, set `RCPPROLL_BENCH_THREADS` to `2` or `14`, and
set the common-workload filter to:

```sh
export RCPPROLL_BENCH_CASES='^(sum|mean|min|max|var|sd)_weighted_(5|20|99)$|^(min|max)_missing_(20|99)_remove_FALSE$|^mean_missing_(20|99)_remove_TRUE$'
```
