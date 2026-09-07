# Short results and unnormalized weights

The baseline is PR revision `b81da02`. The after measurements use the source
in the same commit as this report: products use the direct kernel for at most
16 outputs per column, and unnormalized weights are passed to the kernels
without copying their storage. Sum, mean, variance, and SD retain their
existing dispatch criteria.

Measured on an Apple M4 Pro, macOS arm64, R 4.6.1, Apple Clang 21, with
identical `-O2` builds without OpenMP. Both libraries run in one R process
through the same public wrappers, with one thread. Each timing is the median
of seven paired batches, calibrated to at least 30 ms for the faster version.
Batch order alternates between before/after and after/before. Wrappers and
benchmark closures are explicitly byte-compiled: an identical-library A/A
check exposed a roughly 2 microsecond JIT bias without this step; with it,
the two small weighted controls agreed within 2.5%.

`dispatch-performance.R` defines all inputs and verifies agreement before
measurement. `dispatch-performance.csv` contains the medians and speedups;
`dispatch-performance-batches.csv` contains every measured batch. These are
workload-specific measurements, including ordinary rolling workloads and
normalized-weight controls as well as the targeted cases.

| Case | Before (ms) | After (ms) | Before / after |
| --- | ---: | ---: | ---: |
| Product: 100,000-wide window, 1 output | 0.445 | 0.088 | 5.07x |
| Product: 100,000-wide windows, 4 outputs | 0.527 | 0.094 | 5.62x |
| Product: 100,000-wide windows, 16 outputs | 0.527 | 0.100 | 5.29x |
| Product: 32 columns, width 1,000, 1 output each | 0.156 | 0.032 | 4.85x |
| Unnormalized weighted sum: 1,000,000 weights | 0.625 | 0.516 | 1.21x |
| Unnormalized weighted mean: 1,000,000 weights | 0.672 | 0.547 | 1.23x |
| Control: rolling product, width 1,000 | 0.781 | 0.781 | 1.00x |
| Control: rolling sum, width 1,000 | 0.594 | 0.672 | 0.88x |
| Control: rolling mean, width 1,000 | 0.641 | 0.672 | 0.95x |
| Control: rolling variance, width 1,000 | 0.859 | 0.891 | 0.96x |
| Control: normalized weighted sum, 1,000,000 weights | 3.062 | 3.125 | 0.98x |

Product inputs are `1 + sin(i) * 1e-4`; the rolling controls use 100,000
observations. Weighted inputs use the same values with nonuniform weights
`1 + cos(i) * 0.25`.

The gains are concentrated in short product results and large unnormalized
weighted sums/means. Controls are not uniformly faster: the rolling-sum
control is about 13% slower in this build (0.59 to 0.67 ms), and the rolling
mean/variance controls are about 4–5% slower. An additional identical-library
A/A check on four representative controls differed by at most 3%. The sum
accumulator's add instructions are unchanged apart from addresses; code
placement is a possible explanation, not a confirmed cause. These costs are
reported rather than treated as a universal speedup.


For a million nonuniform unnormalized weights, avoiding the copy also removes
an 8 MB temporary buffer per call. The weights remain owned by the R argument,
which stays rooted until dispatch and all OpenMP workers complete. Normalized
weights still use a separate buffer, and neither mode modifies caller data.

## Numerical constraints on dispatch

Sending few-output sums and means through the direct kernel was faster but
lost their compensated accumulation. For example,
`x <- c(1e16, rep(1, 998), -1e16)` changed the sum from 998 to zero. Those
dispatch changes were discarded.

Direct variance and SD were also faster for a few outputs, but a wider
numerical check found a precision regression. For
`x <- 1e15 + 1 + sin(seq_len(1000000)) * 10`, the direct variance differed
from `var(x - x[1])` by about 2.2e-5 relative, versus near machine precision
with the existing accumulator. Those dispatch changes were discarded too.
Regression tests now preserve these cases.

Products avoid these accumulation tradeoffs. At most 16 outputs use forward
multiplication through the existing direct kernel; longer results retain the
incremental path and its overflow/underflow guard. Low-order rounding can
still differ between direct and grouped multiplication.

## Reproduction

Build the baseline and this revision with the same R/compiler configuration,
installing them into separate libraries, then run from the package root:

```sh
Rscript internal/benchmarks/dispatch-performance.R \
  /path/to/before/RcppRoll/libs/RcppRoll.so \
  /path/to/after/RcppRoll/libs/RcppRoll.so \
  /tmp/dispatch-performance
```

Both DLLs must retain the name `RcppRoll.so` (or `RcppRoll.dll`). Set
`RCPPROLL_BENCH_CASES` to a regular expression to select a subset of cases.
