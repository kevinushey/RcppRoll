# Correctness safeguards and throughput

Measured on macOS arm64, R 4.6.1, Apple Clang 21, identical `-O2` builds
without OpenMP. `correctness-performance.R` loads all libraries in one process
and uses the current public wrappers for each. Each result is the median of
five calibrated batches, in milliseconds per call over 100,000 observations.
The inputs and raw timings are in the adjacent R and CSV files.

The base is `46760a6`; the prior correctness PR is `8c71241`. The final column
includes guarded incremental products and the ordinary unweighted variance
path, with scaling reserved for exceptional totals.

| Case | Base | Prior PR | With safeguards |
| --- | ---: | ---: | ---: |
| Product, width 1,000, factors near one | 0.64 | 5.75 | 0.81 |
| Product, width 10,000, factors near one | 0.61 | 52.00 | 0.77 |
| Product, width 1,000, periodic zeros | 0.66 | 6.00 | 6.63 |
| Variance, width 10 | 0.48 | 1.81 | 0.77 |

These are workload-specific measurements, not universal speedups. The
safeguards retain overhead versus the base. Products with zeros deliberately
use forward multiplication; restoring the old speed for that case would
restore the regrouping behavior this PR fixes.

## Product guard

For each non-missing factor, the accumulator conservatively bounds the
absolute base-two logarithm of its magnitude in integer units of 1/1024.
Near one, `abs(log2(x)) <= 4 * abs(x - 1)` avoids a logarithm call; elsewhere,
the binary exponent supplies a conservative bound. Zeros and infinities
always require the direct path.

When the total bound is below 512, every subset product stays well within the
normal double range. The two-stack grouping may change low-order rounding,
but cannot introduce an overflow or underflow. Integer addition and removal
keep this guard exact as observations enter and leave. Windows outside the
bound use forward multiplication, with consecutive windows batched into
the existing vectorized direct kernel.

## Variance fast path

Unweighted windows first accumulate their ordinary sum with additions only.
Totals outside a conservative range are re-read with scaling before computing
deviations. This includes large finite sums: a rounded mean of constant huge
values could otherwise make their squared deviations overflow. Weighted
variance keeps the scaling and small-weight protections from the prior PR.
