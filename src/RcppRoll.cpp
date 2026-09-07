#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include <algorithm>
#include <cfloat>
#include <climits>
#include <cmath>
#include <cstring>
#include <utility>
#include <vector>

#ifdef _OPENMP
# include <omp.h>
#endif

// Errors longjmp past any C++ frames on the stack, so every Rf_error() here
// fires while validating arguments, before any object owning memory is live.

namespace RcppRoll {

class Fill {

public:

  Fill (SEXP vector) {
    int n = Rf_length(vector);
    if (n == 0) {
      filled_ = false;
      return;
    }

    // Match rep_len(fill, 3): shorter inputs recycle, and longer ones are
    // truncated to the three regions the window layout can use.
    double const* data = REAL(vector);
    left_ = data[0];
    middle_ = data[1 % n];
    right_ = data[2 % n];
    filled_ = true;
  }

Fill (Fill const& other):
  left_(other.left_), middle_(other.middle_), right_(other.right_),
  filled_(other.filled_) {}

inline double left() const { return left_; }
inline double middle() const { return middle_; }
inline double right() const { return right_; }
inline bool filled() const { return filled_; }

private:

  double left_;
  double middle_;
  double right_;
  bool filled_;

};

// How far a window reaches either side of the point it is reported at. Used
// both for the fill padding below and, by the partial routine, to work out
// which observations a window at the edges of 'x' can actually see.
inline int getLeftOffset(char const* align, int n) {
  if (strcmp(align, "left") == 0) {
    return 0;
  } else if (strcmp(align, "center") == 0) {
    return (n - 1) / 2; // round down
  } else if (strcmp(align, "right") == 0) {
    return n - 1;
  } else {
    Rf_error("Invalid 'align'");
  }
  return -1; // silence compiler
}

inline int getRightOffset(char const* align, int n) {
  if (strcmp(align, "left") == 0) {
    return n - 1;
  } else if (strcmp(align, "center") == 0) {
    return n / 2;
  } else if (strcmp(align, "right") == 0) {
    return 0;
  } else {
    Rf_error("Invalid 'align'");
  }
  return -1; // silence compiler
}

inline int getLeftPadding(Fill const& fill, char const* align, int n) {
  if (!fill.filled()) return 0;
  return getLeftOffset(align, n);
}

inline int getRightPadding(Fill const& fill, char const* align, int n) {
  if (!fill.filled()) return 0;
  return getRightOffset(align, n);
}

// How many values the vector routines produce. The padding either side of the
// whole windows always comes to 'n - 1' elements, so 'fill' -- like 'partial'
// -- gives one output per input; otherwise only whole windows are reported.
inline int rollOutputSize(int x_n, int n, int by, Fill const& fill, bool partial) {
  if (partial || fill.filled())
    return x_n;
  // fewer observations than one window means no complete windows at all;
  // integer division would otherwise round the count back up to one when 'by'
  // exceeds the shortfall, and that window would read past the data
  if (x_n < n)
    return 0;
  return (x_n - n) / by + 1;
}

// 'normalize' rescales the weights so that they sum to 'n'. Done once here
// rather than once per column of a matrix, and without touching the caller's
// vector.
inline std::vector<double> normalizeWeights(double const* weights,
                                            int weights_n,
                                            int n,
                                            bool normalize) {

  if (!weights_n)
    return std::vector<double>();
  if (!normalize)
    return std::vector<double>(weights, weights + weights_n);

  // Scale before summing so multiplying every finite weight by a common
  // factor cannot overflow the total and turn all normalized weights to zero.
  // All validation precedes the vector allocation: Rf_error() longjmps past
  // C++ destructors.
  double scale = 0.0;
  for (int i = 0; i < weights_n; ++i) {
    if (!std::isfinite(weights[i]))
      Rf_error("'weights' should be finite when 'normalize = TRUE'");
    double magnitude = fabs(weights[i]);
    if (magnitude > scale)
      scale = magnitude;
  }

  if (scale == 0.0)
    Rf_error("'weights' should have a non-zero sum when 'normalize = TRUE'");

  double total = 0.0;
  double compensation = 0.0;
  for (int i = 0; i < weights_n; ++i) {
    double value = weights[i] / scale;
    double updated = total + value;
    if (fabs(total) >= fabs(value))
      compensation += (total - updated) + value;
    else
      compensation += (value - updated) + total;
    total = updated;
  }
  total += compensation;

  if (total == 0.0 || !std::isfinite(total))
    Rf_error("'weights' should have a finite, non-zero sum when 'normalize = TRUE'");

  for (int i = 0; i < weights_n; ++i) {
    double value = (weights[i] / scale) / total * n;
    if (!std::isfinite(value))
      Rf_error("normalized 'weights' should be finite");
  }

  std::vector<double> scaled(weights_n);
  for (int i = 0; i < weights_n; ++i)
    scaled[i] = (weights[i] / scale) / total * n;

  return scaled;
}

// R's ISNAN and R_FINITE compile to out-of-line calls into libR from C++;
// for doubles these are the same predicates, and they stay inline. Telling
// NA from other NaNs still needs R_IsNA(), so that stays behind an is_nan().
inline bool is_nan(double value) {
  return std::isnan(value);
}

inline bool is_finite(double value) {
  return std::isfinite(value);
}

// sqrt() would turn NA_REAL into a plain NaN, so pass non-values through
inline double window_sqrt(double value) {
  return is_nan(value) ? value : sqrt(value);
}

// Average two ordered middle values without overflowing their sum. For
// opposite signs the sum is safe; for like signs the difference is safe.
inline double midpoint(double lower, double upper) {
  if (is_finite(lower) && is_finite(upper) &&
      std::signbit(lower) == std::signbit(upper))
    return lower + (upper - lower) / 2.0;
  return (lower + upper) / 2.0;
}

// Exceptional path for a mean of finite weighted products whose direct sum
// overflowed. Keep the products' common power of two outside the sum: neither
// forming a product nor restoring its scale may precede the final division.
// Missing observations have already been validated and can only remain here
// under na.rm.
inline double scaled_weighted_mean(double const* x,
                                   double const* weights,
                                   int n,
                                   double denominator) {
  int scale = INT_MIN;
  for (int i = 0; i < n; ++i) {
    if (is_nan(x[i]) || x[i] == 0.0 || weights[i] == 0.0)
      continue;
    int value_exp, weight_exp;
    std::frexp(x[i], &value_exp);
    std::frexp(weights[i], &weight_exp);
    scale = std::max(scale, value_exp + weight_exp);
  }
  if (scale == INT_MIN)
    return 0.0 / denominator;

  double total = 0.0;
  double compensation = 0.0;
  for (int i = 0; i < n; ++i) {
    if (is_nan(x[i]) || x[i] == 0.0 || weights[i] == 0.0)
      continue;
    int value_exp, weight_exp;
    double value = std::frexp(x[i], &value_exp);
    double weight = std::frexp(weights[i], &weight_exp);
    double term = std::ldexp(value * weight, value_exp + weight_exp - scale);
    double updated = total + term;
    if (fabs(total) >= fabs(term))
      compensation += (total - updated) + term;
    else
      compensation += (term - updated) + total;
    total = updated;
  }
  return std::ldexp((total + compensation) / denominator, scale);
}

// Whether every weight is the same, making the weighted call the unweighted
// one: 'normalize' takes any uniform vector to exactly one in real arithmetic,
// and without it only a vector of ones leaves the values untouched. Checked
// against the raw weights, since rescaling them in floating point need not
// land exactly on one. Zero and non-finite weights stay on the weighted path,
// which knows their edge cases.
inline bool weightsAreUniform(double const* weights,
                              int weights_n,
                              bool normalize) {

  if (weights_n == 0)
    return false;

  double first = weights[0];
  for (int i = 1; i < weights_n; ++i)
    if (weights[i] != first)
      return false;

  if (!normalize)
    return first == 1.0;

  return is_finite(first) && first != 0.0;
}

// Neumaier compensated summation. Sliding a window means subtracting values
// that were added earlier, and a plain running total cannot give back the low
// bits of a small value that a much larger one absorbed in the meantime -- the
// compensation term holds on to them, so that a window of wildly different
// magnitudes still totals to what a fresh pass over it would give.
class CompensatedSum {

public:

  CompensatedSum() : total_(0.0), compensation_(0.0), magnitude_(0.0) {}

  void clear() {
    total_ = compensation_ = magnitude_ = 0.0;
  }

  void add(double value) {
    double updated = total_ + value;
    if (is_finite(updated)) {
      if (fabs(total_) >= fabs(value))
        compensation_ += (total_ - updated) + value;
      else
        compensation_ += (value - updated) + total_;
    } else {
      // the total has overflowed, or run into an infinity; there are no low
      // bits left to keep track of, and differencing them would give a NaN
      compensation_ = 0.0;
    }
    total_ = updated;
    double size = fabs(value);
    if (size > magnitude_) magnitude_ = size;
  }

  void remove(double value) {
    add(-value);
  }

  double value() const {
    return total_ + compensation_;
  }

  // Whether the total has lost so much of itself to cancellation that it can
  // no longer be trusted: compensated summation carries roughly the square of
  // double precision, so a total this far below the magnitudes that went into
  // it has nothing left to say. The caller's answer is then to start the
  // window over, which costs what computing it from scratch always cost.
  bool degraded() const {
    double result = value();
    // a total that overflowed sticks at infinity even after the values
    // responsible have left the window, since Inf - finite is still Inf; and
    // an infinity that was added and then taken away again leaves a NaN.
    // Neither says anything about the window that remains.
    if (!is_finite(result))
      return true;
    return magnitude_ > 1e12 * fabs(result);
  }

private:

  double total_;
  double compensation_;
  double magnitude_;

};

// ---------------------------------------------------------------------------
// Window kernels
//
// Reading a window from scratch is a serial chain: each add or compare waits
// on the one before, so the loop runs at the latency of a floating point
// operation however wide the machine's vector units are. The kernels below
// express each operation one observation at a time over T windows held side by
// side, and Reduction<> drives them a strip of windows at a time. The lanes are
// independent, so the compiler turns the lane loops into SIMD -- while each
// window still meets its observations in the order it always did, and so comes
// out to the same bits.
//
// A kernel supplies a State<T> of lanes, init(), one step() per observation in
// unweighted and weighted forms, and finish() for one lane. A two-pass kernel
// also supplies prepare() and step2().
// ---------------------------------------------------------------------------

struct OnePass {
  static const int PASSES = 1;
  template <typename Lanes>
  static void prepare(Lanes&, int, double const*, int, double const*) {}
  template <typename Lanes> static void step2(Lanes&, double const*, int) {}
  template <typename Lanes> static void step2(Lanes&, double const*, int, double) {}
};

// Which non-value a window that summed (or multiplied) to a NaN reports: NA
// where any of its values, or their weights, is NA, and NaN otherwise. The
// lane arithmetic carries one NaN's payload or the other's as the hardware
// sees fit, so this settles the question the way the incremental
// accumulators do.
inline double missing_kind(double const* window, int n, double const* weights) {
  for (int k = 0; k < n; ++k) {
    if (ISNA(window[k]))
      return NA_REAL;
    if (weights && ISNA(weights[k]))
      return NA_REAL;
  }
  return R_NaN;
}

// sum() and mean(). Under na.rm a masked-out value adds -0.0, the one addend
// that leaves every total alone -- adding +0.0 would flip an all-negative-zero
// total's sign -- and what was actually summed, values or weight, is what the
// mean divides by. A count of values is a 64-bit integer: the same width as
// the double lanes, so that stepping it by a truth value is one vector
// subtraction of the comparison's mask, where a double count would have the
// mask converted first.
template <bool NA_RM, bool IS_MEAN>
struct SumKernel : OnePass {

  template <int T>
  struct State {
    double total[T];
    double weight_total[T];
    long long count[T];
  };

  template <int T>
  static void init(State<T>& s) {
    for (int t = 0; t < T; ++t) {
      s.total[t] = 0.0;
      s.weight_total[t] = 0.0;
      s.count[t] = 0;
    }
  }

  template <int T>
  static void step(State<T>& s, double const* p, int stride) {
    for (int t = 0; t < T; ++t) {
      double value = p[t * stride];
      if (NA_RM) {
        bool ok = !is_nan(value);
        s.total[t] += ok ? value : -0.0;
        s.count[t] += ok;
      } else {
        s.total[t] += value;
      }
    }
  }

  template <int T>
  static void step(State<T>& s, double const* p, int stride, double weight) {
    for (int t = 0; t < T; ++t) {
      double value = p[t * stride];
      if (NA_RM) {
        bool ok = !is_nan(value);
        s.total[t] += ok ? value * weight : -0.0;
        s.weight_total[t] += ok ? weight : -0.0;
        s.count[t] += ok;
      } else {
        s.total[t] += value * weight;
      }
    }
  }

  // without na.rm, 'normalize' has already made the weights sum to n
  template <int T>
  static double finish(State<T> const& s,
                       int t,
                       int n,
                       double const* window,
                       double const* weights,
                       bool normalize) {
    double total = s.total[t];
    if (!IS_MEAN) {
      if (!NA_RM && is_nan(total))
        total = missing_kind(window, n, weights);
      return total;
    }

    // Without normalization, a weighted mean remains the arithmetic mean of
    // the weighted values. With it, weights surviving na.rm are normalized
    // again by dividing by their own total.
    double denominator = !NA_RM
      ? (double) n
      : (weights && normalize ? s.weight_total[t] : (double) s.count[t]);

    // Preserve the identity of a genuine missing input before attempting the
    // overflow fallback below. An arithmetic NaN made solely from finite
    // inputs can still have a representable mean.
    if (!NA_RM && is_nan(total)) {
      bool missing = false;
      for (int k = 0; k < n; ++k) {
        if (is_nan(window[k]) || (weights && is_nan(weights[k]))) {
          missing = true;
          break;
        }
      }
      if (missing)
        return missing_kind(window, n, weights);
    }

    double result = total / denominator;
    if (is_finite(result) || is_finite(total))
      return result;

    // Summing finite values can overflow even where their mean is in range.
    // Re-sum values relative to their largest magnitude, dividing before
    // scaling back, so the intermediate total stays representable.
    double scale = 0.0;
    for (int k = 0; k < n; ++k) {
      double value = window[k];
      if (NA_RM && is_nan(value))
        continue;
      if (!is_finite(value) || (weights && !is_finite(weights[k])))
        return result;
      double magnitude = fabs(value);
      if (magnitude > scale)
        scale = magnitude;
    }

    if (scale == 0.0 || !is_finite(denominator))
      return result;

    if (weights && !normalize)
      return scaled_weighted_mean(window, weights, n, denominator);

    double scaled_total = 0.0;
    double compensation = 0.0;
    double lower = R_PosInf;
    double upper = R_NegInf;
    bool bounded = !weights || normalize;
    for (int k = 0; k < n; ++k) {
      double value = window[k];
      if (NA_RM && is_nan(value))
        continue;
      double term = value / scale;
      if (term < lower) lower = term;
      if (term > upper) upper = term;
      if (weights) {
        bounded = bounded && weights[k] >= 0.0;
        term *= weights[k];
      }
      double updated = scaled_total + term;
      if (fabs(scaled_total) >= fabs(term))
        compensation += (scaled_total - updated) + term;
      else
        compensation += (term - updated) + scaled_total;
      scaled_total = updated;
    }
    scaled_total += compensation;

    double scaled_result = scaled_total / denominator;
    if (weights && normalize && lower == upper && denominator != 0.0) {
      scaled_result = lower;
    } else if (bounded && denominator > 0.0) {
      if (scaled_result < lower) scaled_result = lower;
      if (scaled_result > upper) scaled_result = upper;
    }
    return scaled_result * scale;
  }

};

// min() and max(). The selects are the ones the from-scratch loops always
// used, so ties -- and with them the sign of a zero -- resolve as before: min
// keeps the earlier of two equal values, max the later. A NaN loses every
// comparison and so drops out on its own. Without na.rm, each lane records
// whether it saw NA or an ordinary NaN; weighted calls inspect the product,
// since two individually non-missing inputs can still form 0 * Inf.
template <bool NA_RM, bool IS_MIN>
struct ExtremumKernel : OnePass {

  template <int T>
  struct State {
    double value[T];
    char missing[T];
  };

  template <int T>
  static void init(State<T>& s) {
    for (int t = 0; t < T; ++t) {
      s.value[t] = IS_MIN ? R_PosInf : R_NegInf;
      s.missing[t] = 0;
    }
  }

  template <int T>
  static void step(State<T>& s, double const* p, int stride) {
    for (int t = 0; t < T; ++t) {
      double value = p[t * stride];
      if (!NA_RM && is_nan(value)) {
        char kind = ISNA(value) ? 2 : 1;
        if (kind > s.missing[t])
          s.missing[t] = kind;
      }
      s.value[t] = select(value, s.value[t]);
    }
  }

  template <int T>
  static void step(State<T>& s, double const* p, int stride, double weight) {
    for (int t = 0; t < T; ++t) {
      double value = p[t * stride];
      double candidate = value * weight;
      if (!NA_RM && is_nan(candidate)) {
        char kind = ISNA(value) || ISNA(weight) ? 2 : 1;
        if (kind > s.missing[t])
          s.missing[t] = kind;
      }
      s.value[t] = select(candidate, s.value[t]);
    }
  }

  template <int T>
  static double finish(State<T> const& s,
                       int t,
                       int,
                       double const*,
                       double const*,
                       bool) {
    if (!NA_RM && s.missing[t])
      return s.missing[t] == 2 ? NA_REAL : R_NaN;
    return s.value[t];
  }

private:

  static double select(double value, double incumbent) {
    if (IS_MIN)
      return value < incumbent ? value : incumbent;
    // max without na.rm inverted min's comparison, which takes the later of
    // two equal values; the na.rm form asks '>=' so that a NaN loses -- and
    // still takes the later
    if (!NA_RM)
      return value < incumbent ? incumbent : value;
    return value >= incumbent ? value : incumbent;
  }

};

// prod(). A masked-out value multiplies by one, which is exact.
template <bool NA_RM>
struct ProdKernel : OnePass {

  template <int T>
  struct State {
    double product[T];
  };

  template <int T>
  static void init(State<T>& s) {
    for (int t = 0; t < T; ++t)
      s.product[t] = 1.0;
  }

  template <int T>
  static void step(State<T>& s, double const* p, int stride) {
    for (int t = 0; t < T; ++t) {
      double value = p[t * stride];
      s.product[t] *= NA_RM && is_nan(value) ? 1.0 : value;
    }
  }

  template <int T>
  static void step(State<T>& s, double const* p, int stride, double weight) {
    for (int t = 0; t < T; ++t) {
      double value = p[t * stride];
      s.product[t] *= NA_RM && is_nan(value) ? 1.0 : value * weight;
    }
  }

  template <int T>
  static double finish(State<T> const& s,
                       int t,
                       int n,
                       double const* window,
                       double const* weights,
                       bool) {
    double product = s.product[t];
    if (!NA_RM && is_nan(product))
      return missing_kind(window, n, weights);
    return product;
  }

};

// var() and sd(). Corrected two-pass: the deviations are measured from a mean
// that is itself rounded, and their total collects exactly the error that
// introduces -- subtracting it out takes the rounding back out. Without it a
// window whose spread is small beside its mean loses most of its digits.
//
// Weights are frequency weights:
//
//   m  = sum(w * x) / sum(w)
//   s2 = sum(w * (x - m)^2) / (sum(w) - 1)
//
// Unweighted, the count stands in for the weight total, and one finish()
// serves both.
// NAs drop out together with their own weights: a masked-out observation
// contributes a product of zeros to the sums, which is exact, since none of
// the sums can be a negative zero to be disturbed by it. Zeros rather than
// the -0.0 the other kernels add so that the products stay contractible into
// fused multiply-adds, as the loops they replace were.
template <bool NA_RM, bool IS_SD>
struct VarKernel {

  static const int PASSES = 2;

  template <int T>
  struct State {
    double total[T];          // raw sum unweighted, scaled sum when weighted
    double scale[T];          // largest weighted observation magnitude
    double weight_total[T];   // sum of weights relative to 'weight_scale'
    double weight_scale[T];   // largest weight, at least one
    double count[T];          // values that were not NaN
    double mean[T];
    double squares[T];
    double residual[T];
  };

  template <int T>
  static void init(State<T>& s) {
    for (int t = 0; t < T; ++t) {
      s.total[t] = 0.0;
      s.scale[t] = 0.0;
      s.weight_total[t] = 0.0;
      s.weight_scale[t] = 1.0;
      s.count[t] = 0.0;
      s.mean[t] = 0.0;
      s.squares[t] = 0.0;
      s.residual[t] = 0.0;
    }
  }

  // Unweighted, the weight total is the count. The counts are doubles here,
  // stepped by a select between constants: this kernel measured faster that
  // way than with the integer lanes the others use.
  template <int T>
  static void step(State<T>& s, double const* p, int stride) {
    for (int t = 0; t < T; ++t) {
      double value = p[t * stride];
      bool ok = !is_nan(value);
      s.total[t] += ok ? value : -0.0;
      s.weight_total[t] += ok ? 1.0 : 0.0;
    }
  }

  template <int T>
  static void step(State<T>& s, double const* p, int stride, double weight) {
    for (int t = 0; t < T; ++t) {
      double value = p[t * stride];
      bool ok = !is_nan(value);
      bool contributes = ok && weight != 0.0;
      if (contributes) {
        if (weight > s.weight_scale[t]) {
          double ratio = s.weight_scale[t] / weight;
          s.total[t] *= ratio;
          s.weight_total[t] *= ratio;
          s.weight_scale[t] = weight;
        }
        double magnitude = fabs(value);
        if (magnitude > s.scale[t]) {
          s.total[t] *= s.scale[t] / magnitude;
          s.scale[t] = magnitude;
        }
        double scaled = s.scale[t] != 0.0 ? value / s.scale[t] : value;
        s.total[t] += (weight / s.weight_scale[t]) * scaled;
      }
      s.weight_total[t] += ok ? weight / s.weight_scale[t] : -0.0;
      s.count[t] += ok ? 1.0 : 0.0;
    }
  }

  template <int T>
  static void prepare(State<T>& s, int n, double const* p, int stride,
                      double const* weights) {
    if (weights) {
      for (int t = 0; t < T; ++t)
        s.mean[t] = (s.total[t] / s.weight_total[t]) * s.scale[t];
      return;
    }
    // Keep the ordinary divisions together so the compiler can vectorize
    // them independently of the exceptional-lane fallback.
    bool ordinary = true;
    for (int t = 0; t < T; ++t)
      ordinary &= fabs(s.total[t]) <= 1e150;
    for (int t = 0; t < T; ++t)
      s.mean[t] = s.total[t] / s.weight_total[t];
    if (ordinary)
      return;
    for (int t = 0; t < T; ++t) {
      if (!(fabs(s.total[t]) <= 1e150)) {
        // Ordinary windows need only additions in the first pass. Re-read
        // exceptional lanes when the total overflowed or is large enough
        // that rounding the mean could overflow its squared deviations.
        double total = 0.0;
        double scale = 0.0;
        for (int k = 0; k < n; ++k) {
          double value = p[t * stride + k];
          if (is_nan(value)) continue;
          double magnitude = fabs(value);
          if (magnitude > scale) {
            total *= scale / magnitude;
            scale = magnitude;
          }
          total += scale != 0.0 ? value / scale : value;
        }
        s.mean[t] = (total / s.weight_total[t]) * scale;
      }
    }
  }

  template <int T>
  static void step2(State<T>& s, double const* p, int stride) {
    for (int t = 0; t < T; ++t) {
      double value = p[t * stride];
      double difference = is_nan(value) ? 0.0 : value - s.mean[t];
      s.squares[t] += difference * difference;
      s.residual[t] += difference;
    }
  }

  template <int T>
  static void step2(State<T>& s, double const* p, int stride, double weight) {
    for (int t = 0; t < T; ++t) {
      double value = p[t * stride];
      bool ok = !is_nan(value);
      double w = ok ? weight / s.weight_scale[t] : 0.0;
      double difference = ok && weight != 0.0 ? value - s.mean[t] : 0.0;
      if (ok && weight > 0.0 && w < DBL_MIN && is_finite(difference)) {
        // A tiny weight ratio can underflow even when multiplying it by a
        // large deviation yields a substantial contribution. Combine the
        // exponents before rounding either moment back to a double.
        int weight_exp, scale_exp, difference_exp;
        double weight_part = std::frexp(weight, &weight_exp);
        double scale_part = std::frexp(s.weight_scale[t], &scale_exp);
        double difference_part = std::frexp(difference, &difference_exp);
        double moment = (weight_part / scale_part) * difference_part;
        int exponent = weight_exp - scale_exp + difference_exp;
        s.residual[t] += std::ldexp(moment, exponent);
        s.squares[t] +=
          std::ldexp(moment * difference_part, exponent + difference_exp);
      } else {
        s.squares[t] += w * difference * difference;
        s.residual[t] += w * difference;
      }
    }
  }

  template <int T>
  static double finish(State<T> const& s,
                       int t,
                       int n,
                       double const*,
                       double const* weights,
                       bool) {

    double count = weights ? s.count[t] : s.weight_total[t];
    if (!NA_RM && count != n)
      return NA_REAL;

    // NA for fewer than two values, matching var() on a vector of length 0 or
    // 1; a weight total that is zero or negative (possible with 'normalize =
    // FALSE', or after dropping NAs) has no meaningful answer either
    double weight = s.weight_total[t];
    double unit = weights ? 1.0 / s.weight_scale[t] : 1.0;
    double result;
    if (count < 2 || !(weight > unit)) {
      result = NA_REAL;
    } else if (s.squares[t] == R_PosInf) {
      // the deviations squared past what a double can hold, so the variance
      // is out of range too -- and the correction would only turn it into a
      // NaN
      result = R_PosInf;
    } else {
      double total =
        s.squares[t] - s.residual[t] * (s.residual[t] / weight);
      if (total < 0.0) total = 0.0;
      result = total / (weight - unit);
    }

    return IS_SD ? window_sqrt(result) : result;
  }

};

// Drives a kernel over one window, or over a strip of them. A strip walks
// WIDTH windows abreast.
template <typename Kernel>
struct Reduction {

  Reduction() : normalize_(true) {}

  void setNormalize(bool normalize) {
    normalize_ = normalize;
  }

  // Wide enough that the lanes' arithmetic runs ahead of the loads feeding
  // it; wider only spills the lanes out of registers.
  static const int WIDTH = 16;

  double operator()(double const* x, int offset, int n) const {
    double result;
    run<1, 1>(x + offset, 1, n, (double const*) NULL, &result, 1);
    return result;
  }

  double operator()(double const* x,
                    int offset,
                    double const* weights,
                    int n) const {
    double result;
    run<1, 1>(x + offset, 1, n, weights, &result, 1);
    return result;
  }

  // 'count' windows of width 'n' starting at 'start' and every 'by' after it,
  // written to out[0], out[stride], ...
  void strip(double const* x,
             int start,
             int by,
             int n,
             double const* weights,
             int count,
             double* out,
             int stride) const {

    double const* p = x + start;
    int j = 0;

    // A strip's tail, and a short strip -- a matrix of short columns is made
    // of them -- still run a few windows abreast. Windows 'by' apart have
    // their lanes gathered rather than loaded whole, and measured best at
    // half the width.
    if (by == 1) {
      for (; j + WIDTH <= count; j += WIDTH)
        run<WIDTH, 1>(p + j, 1, n, weights, out + j * stride, stride);
      for (; j + 4 <= count; j += 4)
        run<4, 1>(p + j, 1, n, weights, out + j * stride, stride);
    } else {
      for (; j + WIDTH / 2 <= count; j += WIDTH / 2)
        run<WIDTH / 2, 0>(p + j * by, by, n, weights, out + j * stride, stride);
      for (; j + 4 <= count; j += 4)
        run<4, 0>(p + j * by, by, n, weights, out + j * stride, stride);
    }

    for (; j < count; ++j)
      run<1, 1>(p + j * by, 1, n, weights, out + j * stride, stride);

  }

private:

  // T windows abreast, neighbouring windows' observations STRIDE apart -- or
  // 'by' apart where STRIDE is zero. A constant stride is what lets the
  // compiler fetch a step's observations as one vector.
  template <int T, int STRIDE>
  void run(double const* p,
           int by,
           int n,
           double const* weights,
           double* out,
           int stride_out) const {

    int stride = STRIDE ? STRIDE : by;

    typename Kernel::template State<T> state;
    Kernel::init(state);

    if (weights) {
      for (int k = 0; k < n; ++k)
        Kernel::step(state, p + k, stride, weights[k]);
    } else {
      for (int k = 0; k < n; ++k)
        Kernel::step(state, p + k, stride);
    }

    if (Kernel::PASSES == 2) {
      Kernel::prepare(state, n, p, stride, weights);
      if (weights) {
        for (int k = 0; k < n; ++k)
          Kernel::step2(state, p + k, stride, weights[k]);
      } else {
        for (int k = 0; k < n; ++k)
          Kernel::step2(state, p + k, stride);
      }
    }

    for (int t = 0; t < T; ++t)
      out[t * stride_out] =
        Kernel::finish(state, t, n, p + t * stride, weights, normalize_);

  }

  bool normalize_;

};

// ---------------------------------------------------------------------------
// Windowing functions
//
// These compute a window from scratch, or a strip of windows abreast. They
// still carry the weighted forms, which have no incremental equivalent -- a
// weight belongs to a position within the window, so sliding the window
// re-pairs every weight with a different observation.
// ---------------------------------------------------------------------------

template <bool NA_RM>
struct mean_f : Reduction< SumKernel<NA_RM, true> > {};

template <bool NA_RM>
struct sum_f : Reduction< SumKernel<NA_RM, false> > {};

template <bool NA_RM>
struct min_f : Reduction< ExtremumKernel<NA_RM, true> > {};

template <bool NA_RM>
struct max_f : Reduction< ExtremumKernel<NA_RM, false> > {};

template <bool NA_RM>
struct prod_f : Reduction< ProdKernel<NA_RM> > {};

template <bool NA_RM>
struct var_f : Reduction< VarKernel<NA_RM, false> > {};

template <bool NA_RM>
struct sd_f : Reduction< VarKernel<NA_RM, true> > {};

// Variance treats weights as repeat counts, for which negative or non-finite
// values have no statistical meaning. Other operations retain their broader
// historical weight semantics.
inline void validate_frequency_weights(double const* weights, int n) {
  for (int i = 0; i < n; ++i)
    if (!is_finite(weights[i]) || weights[i] < 0.0)
      Rf_error("'weights' should be finite and non-negative for variance");
}

template <typename Callable>
inline void validate_weights(Callable, double const*, int) {}

template <bool NA_RM>
inline void validate_weights(var_f<NA_RM>, double const* weights, int n) {
  validate_frequency_weights(weights, n);
}

template <bool NA_RM>
inline void validate_weights(sd_f<NA_RM>, double const* weights, int n) {
  validate_frequency_weights(weights, n);
}

// The strip form of a function with no lane-wise kernel: one window at a time.
template <typename Callable>
inline void strip_singly(Callable& f,
                         double const* x,
                         int start,
                         int by,
                         int n,
                         double const* weights,
                         int count,
                         double* out,
                         int stride) {

  for (int j = 0; j < count; ++j) {
    int offset = start + j * by;
    out[j * stride] = weights ? f(x, offset, weights, n) : f(x, offset, n);
  }

}

// The sorted form of the weighted median: order the window, then walk the
// cumulative weight up to the crossing. Kept as the fallback for weights the
// selection below cannot order -- a negative, NaN, or infinite weight makes
// the cumulative weight non-monotonic, or not a number, and this scan's
// crossing is then the defined answer.
inline double weighted_median_scan(
    std::vector< std::pair<double, double> >& scratch) {

  std::sort(scratch.begin(), scratch.end());

  double weights_sum = 0.0;
  for (size_t i = 0; i < scratch.size(); ++i)
    weights_sum += scratch[i].second;

  // guard against zero, negative, or non-finite weight sums, which would
  // otherwise let the search below run past the end of the window
  if (!(weights_sum > 0))
    return NA_REAL;

  size_t k = 0;
  double remaining = weights_sum;
  for (; k + 1 < scratch.size(); ++k) {
    remaining -= scratch[k].second;
    if (!(remaining > weights_sum / 2))
      break;
  }

  return scratch[k].first;

}

// Compute a weighted median, ignoring any NAs in the window: the smallest
// value whose cumulative weight, taken in value order, reaches half the total.
// The weights are tied to their associated values first, since a weight
// applies to the value at its own position rather than to the value that ends
// up ranked there. 'scratch' and 'spare' belong to the caller, so that a pass
// over many windows reuses two buffers rather than allocating for each window.
//
// Rather than sorting the window to walk its cumulative weight, this
// partitions around a pivot and descends into the part holding the crossing:
// expected linear time in the window size, where the sort pays an extra log
// factor. Partitioning writes into the spare buffer rather than swapping in
// place -- each pair is read once and written at most once, and the pivot's
// run, which is only ever reported and never descended into, is not moved at
// all.
inline double weighted_median(double const* x,
                              int offset,
                              double const* weights,
                              int n,
                              std::vector< std::pair<double, double> >& scratch,
                              std::vector< std::pair<double, double> >& spare) {

  scratch.clear();
  bool orderly = true;
  double weights_sum = 0.0;
  for (int i = 0; i < n; ++i) {
    double value = x[offset + i];
    if (is_nan(value))
      continue;
    double weight = weights[i];
    if (!(weight >= 0.0) || !is_finite(weight))
      orderly = false;
    weights_sum += weight;
    scratch.push_back(std::make_pair(value, weight));
  }

  if (scratch.empty())
    return NA_REAL;

  // the descent below leans on a cumulative weight that only ever grows, and
  // on comparisons that mean what they say; a weight set (or total) that
  // cannot promise that takes the scan
  if (!orderly || !is_finite(weights_sum))
    return weighted_median_scan(scratch);

  // guard against an all-zero weight total, which has no crossing to find
  if (!(weights_sum > 0))
    return NA_REAL;

  double target = weights_sum / 2;

  if (spare.size() < scratch.size())
    spare.resize(scratch.size());

  std::pair<double, double>* from = &scratch[0];
  std::pair<double, double>* into = &spare[0];
  size_t size = scratch.size();
  double below = 0.0; // total weight of the values ranked before 'from'

  while (size > 1) {

    // median-of-three pivot: windows of already-ordered data are common, and
    // an end-of-range pivot would descend one element at a time through them
    double a = from[0].first;
    double b = from[size / 2].first;
    double c = from[size - 1].first;
    double pivot = a < b
      ? (b < c ? b : (a < c ? c : a))
      : (a < c ? a : (b < c ? c : b));

    // Split off the values below and above the pivot at the two ends of the
    // other buffer, keeping the pivot's run whole so that repeated values
    // cannot stall the descent. Which side a value lands on is a coin flip no
    // branch predictor can learn, so the split is branchless: every element
    // is written to both frontiers, and only the matching side's counter --
    // and weight -- moves. A slot past its counter holds a stale copy that
    // the next committed write overwrites, and the span left between the two
    // frontiers is never read.
    size_t n_lt = 0;
    size_t n_gt = 0;
    double weight_lt = 0.0;
    double weight_eq = 0.0;

    for (size_t i = 0; i < size; ++i) {
      double value = from[i].first;
      double weight = from[i].second;
      bool lt = value < pivot;
      bool gt = pivot < value;
      into[n_lt] = from[i];
      into[size - 1 - n_gt] = from[i];
      n_lt += lt;
      n_gt += gt;
      weight_lt += lt ? weight : 0.0;
      weight_eq += (lt | gt) ? 0.0 : weight;
    }

    // Descend into whichever part the cumulative weight crosses half within,
    // the buffer just read becoming the next level's writing room. The values
    // below the pivot never hold the crossing when they are empty: reusing
    // 'after_eq' as the next 'below' keeps the comparison that sent the
    // descent rightwards bit-identical to the one guarding the left.
    double after_lt = below + weight_lt;
    double after_eq = after_lt + weight_eq;

    std::pair<double, double>* room = from;
    if (!(weights_sum - after_lt > target)) {
      from = into;
      size = n_lt;
    } else if (n_gt && weights_sum - after_eq > target) {
      below = after_eq;
      from = into + (size - n_gt);
      size = n_gt;
    } else {
      return pivot;
    }
    into = room;

  }

  return from[0].first;

}

// Select the median out of 'scratch', which this reorders. std::nth_element
// places the middle value in linear time, where a partial sort of the lower
// half of the window would cost an extra log factor. The lower form reports
// the lower of an even window's two middle values rather than their average,
// which is the value a weighted median with uniform weights selects.
inline double select_median(std::vector<double>& scratch, bool lower) {

  size_t n = scratch.size();
  if (n == 0)
    return NA_REAL;

  if (lower) {
    std::nth_element(
      scratch.begin(), scratch.begin() + (n - 1) / 2, scratch.end());
    return scratch[(n - 1) / 2];
  }

  std::nth_element(
    scratch.begin(), scratch.begin() + n / 2, scratch.end());
  double upper = scratch[n / 2];

  if (n % 2 == 0) {
    // everything below the midpoint is already partitioned below it, so the
    // other middle value is simply the largest of that part
    double lower_middle =
      *std::max_element(scratch.begin(), scratch.begin() + n / 2);
    return midpoint(lower_middle, upper);
  }

  return upper;

}

// The weighted forms select an observation whatever LOWER says: a weighted
// median never interpolates, so it is its own lower form.
template <bool NA_RM, bool LOWER = false>
struct median_f;

template <bool LOWER>
struct median_f<false, LOWER> {

  void setNormalize(bool) {}

  inline double operator()(double const* x, int offset, int n) {

    for (int i = offset; i < offset + n; i++)
      if (is_nan(x[i]))
        return NA_REAL;

    scratch_.assign(x + offset, x + offset + n);
    return select_median(scratch_, LOWER);

  }

  inline double operator()(double const* x,
                           int offset,
                           double const* weights,
                           int n) {

    for (int i = offset; i < offset + n; i++)
      if (is_nan(x[i]))
        return NA_REAL;

    return weighted_median(x, offset, weights, n,
                           weighted_scratch_, weighted_spare_);
  }

  void strip(double const* x,
             int start,
             int by,
             int n,
             double const* weights,
             int count,
             double* out,
             int stride) {
    strip_singly(*this, x, start, by, n, weights, count, out, stride);
  }

private:

  std::vector<double> scratch_;
  std::vector< std::pair<double, double> > weighted_scratch_;
  std::vector< std::pair<double, double> > weighted_spare_;

};

template <bool LOWER>
struct median_f<true, LOWER> {

  void setNormalize(bool) {}

  inline double operator()(double const* x, int offset, int n) {

    scratch_.clear();
    scratch_.reserve(n);
    for (int i = offset; i < offset + n; i++)
      if (!is_nan(x[i]))
        scratch_.push_back(x[i]);

    return select_median(scratch_, LOWER);

  }

  inline double operator()(double const* x,
                           int offset,
                           double const* weights,
                           int n) {

    return weighted_median(x, offset, weights, n,
                           weighted_scratch_, weighted_spare_);
  }

  void strip(double const* x,
             int start,
             int by,
             int n,
             double const* weights,
             int count,
             double* out,
             int stride) {
    strip_singly(*this, x, start, by, n, weights, count, out, stride);
  }

private:

  std::vector<double> scratch_;
  std::vector< std::pair<double, double> > weighted_scratch_;
  std::vector< std::pair<double, double> > weighted_spare_;

};

// Whether carrying a window forward beats reading strips of windows from
// scratch. The crossovers were measured against the lane-wise kernels:
// 'contiguous' is the window size at which the two cost the same for a 'by'
// of one, where a strip loads its lanes as vectors. A strip of windows
// further apart has to gather its lanes, which costs more per observation,
// so 'strided' -- in multiples of 'by' -- is lower. Either way a 'by' past
// the crossover includes every 'by' wide enough to leave gaps between the
// windows, where there is nothing to carry forward at all.
inline bool incrementalWins(int n, int by, int contiguous, int strided) {
  if (by == 1)
    return n >= contiguous;
  return n >= strided * (long long) by;
}

// ---------------------------------------------------------------------------
// Windowed accumulators
//
// Computing each window from scratch costs O(n) per point. The accumulators
// below instead carry the state of one window forward, paying only for the
// observations that enter and leave it. That works because every sequence of
// windows generated here moves monotonically: neither edge ever steps
// backwards, whatever 'by', 'align' and 'partial' are set to.
//
// Every accumulator answers to the same two calls: compute(start, end) for
// one window, of any width, and computeStrip() for a run of whole windows a
// fixed 'by' apart. The drivers hand each chunk over as a strip, so that an
// accumulator with a better way to walk a run of windows -- the block scans
// behind min() and max(), or the lane-wise kernels above -- gets to use it.
// ---------------------------------------------------------------------------

// Drives one accumulator over a sequence of windows. Derived classes supply
// clear() / add() / remove() / value(); this is a compile-time (CRTP) base, so
// none of that costs a virtual call in the inner loop.
template <typename Derived>
class WindowAccumulator {

public:

  WindowAccumulator(double const* x, int n)
    : x_(x), n_(n), start_(0), end_(-1), credit_(0) {}

  double compute(int start, int end) {

    Derived& self = static_cast<Derived&>(*this);

    // a 'by' wider than the window leaves gaps, with nothing to carry forward
    if (start > end_ + 1) {
      self.clear();
      start_ = start;
      end_ = start - 1;
    }

    while (start_ < start) {
      self.remove(start_);
      ++start_;
      ++credit_;
    }

    while (end_ < end) {
      ++end_;
      self.add(end_);
      ++credit_;
    }

    // Sliding a window subtracts values that were added earlier, which no
    // amount of care makes safe when the window spans magnitudes far enough
    // apart. Where that has happened, fall back to reading the window itself.
    //
    // A total that is merely ill-conditioned earns that rebuild at most once
    // per window's worth of slides: compensated arithmetic loses so little
    // per operation that rebuilding more often cannot change the digits, and
    // data whose windows genuinely cancel -- where the check would otherwise
    // fire at every point forever -- would pay a full rebuild each time. A
    // total that has stopped being a number at all is rebuilt immediately.
    if (start_ <= end_ && self.degraded()) {
      if (self.urgent() || credit_ >= end_ - start_ + 1) {
        self.clear();
        self.prepare(start_, end_);
        for (int i = start_; i <= end_; ++i)
          self.add(i);
        credit_ = 0;
      }
    }

    return self.value();
  }

  // a run of whole windows is walked one slide at a time
  void computeStrip(int start, int by, int count, double* out, int stride) {
    for (int j = 0; j < count; ++j) {
      int first = start + j * by;
      out[j * stride] = compute(first, first + n_ - 1);
    }
  }

protected:

  double const* x_;
  int n_;
  int start_;
  int end_;
  int credit_;

};

// Recomputes the window on every call: the fallback whenever carrying state
// forward is not worthwhile, and the only form an operation without an
// incremental equivalent ever takes. A strip goes to the function's own strip
// form, which for the kernels above walks its windows abreast.
template <typename Callable>
class DirectAccumulator {

public:

  DirectAccumulator(Callable f, double const* x, int n)
    : f_(f), x_(x), n_(n) {}

  // an operation with no incremental form always reads the window afresh
  static bool worthwhile(int, int, int) { return false; }

  double compute(int start, int end) {
    return f_(x_, start, end - start + 1);
  }

  void computeStrip(int start, int by, int count, double* out, int stride) {
    f_.strip(x_, start, by, n_, (double const*) NULL, count, out, stride);
  }

private:

  Callable f_;
  double const* x_;
  int n_;

};

// The weighted form of a windowing function. Weighted windows are never
// clipped -- 'weights' is rejected together with 'partial' -- so the width is
// always the weights' own length.
template <typename Callable>
class WeightedAccumulator {

public:

  WeightedAccumulator(Callable f, double const* x, double const* weights, int n)
    : f_(f), x_(x), weights_(weights), n_(n) {}

  double compute(int start, int end) {
    return f_(x_, start, weights_, end - start + 1);
  }

  void computeStrip(int start, int by, int count, double* out, int stride) {
    f_.strip(x_, start, by, n_, weights_, count, out, stride);
  }

private:

  Callable f_;
  double const* x_;
  double const* weights_;
  int n_;

};

// Running total behind sum() and mean(). Infinities are counted rather than
// accumulated: one that entered and later left a window would otherwise poison
// the total with a NaN it could never recover from.
template <bool NA_RM, bool IS_MEAN>
class SumAccumulator :
  public WindowAccumulator< SumAccumulator<NA_RM, IS_MEAN> > {

  typedef WindowAccumulator< SumAccumulator<NA_RM, IS_MEAN> > Base;

public:

  template <typename Callable>
  SumAccumulator(Callable, double const* x, int n) : Base(x, n) {
    clear();
  }

  // one add and one subtract per observation entering or leaving, against a
  // strip that under na.rm masks each observation, and for a mean counts it
  static bool worthwhile(int n, int by, int) {
    int contiguous = NA_RM ? (IS_MEAN ? 52 : 64) : 128;
    int strided = NA_RM ? (IS_MEAN ? 28 : 36) : 64;
    return incrementalWins(n, by, contiguous, strided);
  }

  bool degraded() const { return total_.degraded(); }

  // an overflowed or NaN total is not stale digits but a wrong value, and
  // removals can never repair it -- it must not wait for rebuild credit
  bool urgent() const { return !is_finite(total_.value()); }

  void prepare(int, int) {}

  void clear() {
    total_.clear();
    n_finite_ = n_na_ = n_nan_ = n_pos_inf_ = n_neg_inf_ = 0;
  }

  void add(int i) {
    double value = this->x_[i];
    if (is_nan(value)) { if (ISNA(value)) ++n_na_; else ++n_nan_; }
    else if (value == R_PosInf) ++n_pos_inf_;
    else if (value == R_NegInf) ++n_neg_inf_;
    else { total_.add(value); ++n_finite_; }
  }

  void remove(int i) {
    double value = this->x_[i];
    if (is_nan(value)) { if (ISNA(value)) --n_na_; else --n_nan_; }
    else if (value == R_PosInf) --n_pos_inf_;
    else if (value == R_NegInf) --n_neg_inf_;
    else { total_.remove(value); --n_finite_; }
  }

  double value() const {

    // NA and NaN are counted apart so that each keeps its identity, as it
    // did when the values were simply added up; a window holding both kinds
    // reports NA, where summation order used to decide
    if (!NA_RM && (n_na_ || n_nan_))
      return n_na_ ? NA_REAL : R_NaN;

    double result;
    if (n_pos_inf_ && n_neg_inf_) result = R_NaN;
    else if (n_pos_inf_) result = R_PosInf;
    else if (n_neg_inf_) result = R_NegInf;
    else result = total_.value();

    if (!IS_MEAN)
      return result;

    // A finite window can have an overflowing running sum but a representable
    // mean. Re-sum only that exceptional case after scaling the observations;
    // the common path above remains bit-for-bit unchanged.
    if (!is_finite(result) && !n_pos_inf_ && !n_neg_inf_) {
      double scale = 0.0;
      for (int i = this->start_; i <= this->end_; ++i) {
        double value = this->x_[i];
        if (is_finite(value) && fabs(value) > scale)
          scale = fabs(value);
      }
      if (scale > 0.0) {
        double scaled_total = 0.0;
        for (int i = this->start_; i <= this->end_; ++i) {
          double value = this->x_[i];
          if (is_finite(value))
            scaled_total += value / scale;
        }
        return (scaled_total / n_finite_) * scale;
      }
    }

    // mean() divides by the number of values it actually saw; an empty window
    // gives the NaN that 0 / 0 produces, as it did before
    return result / (n_finite_ + n_pos_inf_ + n_neg_inf_);
  }

private:

  CompensatedSum total_;
  int n_finite_;
  int n_na_;
  int n_nan_;
  int n_pos_inf_;
  int n_neg_inf_;

};

// Running sums behind var() and sd(). Held about a shift near the window mean,
// which keeps 's1_' small and so keeps the sum-of-squares form below from
// cancelling; the shift is re-chosen every time the window is rebuilt.
template <bool NA_RM, bool IS_SD>
class VarAccumulator :
  public WindowAccumulator< VarAccumulator<NA_RM, IS_SD> > {

  typedef WindowAccumulator< VarAccumulator<NA_RM, IS_SD> > Base;

public:

  template <typename Callable>
  VarAccumulator(Callable, double const* x, int n) : Base(x, n) {
    clear();
  }

  // two running sums against two passes over the window
  static bool worthwhile(int n, int by, int) {
    return incrementalWins(n, by, 28, 10);
  }

  bool degraded() const {

    // the sum of squares settles the answer's magnitude, and a large
    // deviation leaving the window is what puts it out of reach
    if (s2_.degraded())
      return true;

    // the variance comes out of 's2 - s1 * s1 / count', and that subtraction
    // only keeps its digits while the shift stays near the mean it is standing
    // in for. Once the two terms are of a size, re-centre on the window.
    double s1 = s1_.value();
    return s1 * s1 > 0.25 * n_finite_ * s2_.value();
  }

  // as for the running total: a non-finite sum of squares is a wrong value,
  // not lost precision, and only a rebuild recovers it
  bool urgent() const {
    return !is_finite(s1_.value()) || !is_finite(s2_.value());
  }

  // Centre on the window's own mean. Deviations are then as small as they can
  // be, which is what keeps both the running sums and the subtraction above
  // well conditioned; add() would otherwise fall back on the first value it
  // saw, which can sit arbitrarily far from the mean.
  void prepare(int start, int end) {
    double scale = 0.0;
    double scaled_total = 0.0;
    int count = 0;
    for (int i = start; i <= end; ++i) {
      double value = this->x_[i];
      if (is_finite(value)) {
        double magnitude = fabs(value);
        if (magnitude > scale) {
          scaled_total *= scale / magnitude;
          scale = magnitude;
        }
        scaled_total += scale != 0.0 ? value / scale : value;
        ++count;
      }
    }
    shift_ = count ? (scaled_total / count) * scale : 0.0;
    have_shift_ = true;
  }

  void clear() {
    s1_.clear();
    s2_.clear();
    shift_ = 0.0;
    have_shift_ = false;
    n_finite_ = n_na_ = n_infinite_ = 0;
  }

  void add(int i) {
    double value = this->x_[i];
    if (is_nan(value)) { ++n_na_; return; }
    if (!is_finite(value)) { ++n_infinite_; return; }
    if (!have_shift_) { shift_ = value; have_shift_ = true; }
    double difference = value - shift_;
    s1_.add(difference);
    s2_.add(difference * difference);
    ++n_finite_;
  }

  void remove(int i) {
    double value = this->x_[i];
    if (is_nan(value)) { --n_na_; return; }
    if (!is_finite(value)) { --n_infinite_; return; }
    double difference = value - shift_;
    s1_.remove(difference);
    s2_.remove(difference * difference);
    --n_finite_;
  }

  double value() const {
    double result = variance();
    return IS_SD ? window_sqrt(result) : result;
  }

private:

  double variance() const {

    if (!NA_RM && n_na_)
      return NA_REAL;

    // var() is NA for a vector of length 0 or 1 -- infinities included, so
    // this has to be settled before they are
    int count = n_finite_ + n_infinite_;
    if (count < 2)
      return NA_REAL;

    // a window holding an infinity has an infinite mean, so at least one
    // deviation from it is NaN
    if (n_infinite_)
      return R_NaN;

    // the deviations squared to more than a double can hold, so the variance
    // is out of range too -- and subtracting from that infinity below would
    // only turn it into a NaN
    double s2 = s2_.value();
    if (s2 == R_PosInf)
      return R_PosInf;

    double s1 = s1_.value();
    double mean = s1 / n_finite_;
    double total = s2 - s1 * mean;
    if (total < 0.0) total = 0.0;
    return total / (n_finite_ - 1);
  }

  CompensatedSum s1_;
  CompensatedSum s2_;
  double shift_;
  bool have_shift_;
  int n_finite_;
  int n_na_;
  int n_infinite_;

};

// Block scans behind min() and max() (van Herk / Gil-Werman). Cut the data
// into blocks of n: a window of n then spans the tail of one block and the
// head of the next, so its extremum is the extremum of just two values -- a
// suffix extremum of the first block and a prefix extremum of the second. One
// pass each way over every block computes them all: three comparisons per
// observation whatever the window size, no window state to carry, and no
// branch that depends on the data.
//
// Ties are broken as the from-scratch loops break them, so that the sign of a
// zero carries through unchanged: min keeps the earlier of two equal values,
// max the later. The pass that walks against the data's order asks strictly
// where the pass that walks with it does not, and where the two halves meet
// the same side wins. A NaN loses every comparison and so drops out on its
// own; without na.rm, each pass also carries which kind it has met.
template <bool NA_RM, bool IS_MIN>
class ExtremumAccumulator {

public:

  template <typename Callable>
  ExtremumAccumulator(Callable, double const* x, int n) : x_(x), n_(n) {
    suffix_.resize(n > 0 ? n : 1);
    suffix_na_.resize(n > 0 ? n : 1);
  }

  // three comparisons per observation of the strip's span, against a strip
  // that pays n comparisons per window -- and, without na.rm, counts too
  static bool worthwhile(int n, int by, int) {
    return incrementalWins(n, by, NA_RM ? 12 : 8, NA_RM ? 3 : 1);
  }

  // a window clipped at the data's edge is read on its own
  double compute(int start, int end) {
    return Reduction< ExtremumKernel<NA_RM, IS_MIN> >()(
      x_, start, end - start + 1);
  }

  void computeStrip(int start, int by, int count, double* out, int stride) {

    double const* x = x_ + start;
    int n = n_;
    int span = (count - 1) * by + n;
    double* suffix = &suffix_[0];
    char* suffix_na = &suffix_na_[0];

    int j = 0;
    for (int block = 0; j < count; block += n) {

      // the block's suffix extrema, walking back: each is the extremum from
      // its position to the block's end. The last block may be cut short by
      // the data, but no window starts in one that is.
      int block_end = block + n < span ? block + n : span;
      double running = identity();
      char missing = 0;
      for (int i = block_end - 1; i >= block; --i) {
        double value = x[i];
        running = backward(value, running);
        suffix[i - block] = running;
        if (!NA_RM) {
          if (is_nan(value)) {
            char kind = ISNA(value) ? 2 : 1;
            if (kind > missing)
              missing = kind;
          }
          suffix_na[i - block] = missing;
        }
      }

      // The windows starting in this block. The first may be the block
      // itself; the rest reach into the next block, over which a prefix
      // extremum walks forward as far as each window needs.
      double prefix = identity();
      char prefix_missing = 0;
      int next = block + n;
      for (; j < count; ++j) {

        int first = j * by;
        if (first >= block + n)
          break;

        double result;
        char result_missing;
        if (first == block) {
          result = suffix[0];
          result_missing = !NA_RM ? suffix_na[0] : 0;
        } else {
          int last = first + n - 1;
          for (; next <= last; ++next) {
            double value = x[next];
            prefix = forward(value, prefix);
            if (!NA_RM && is_nan(value)) {
              char kind = ISNA(value) ? 2 : 1;
              if (kind > prefix_missing)
                prefix_missing = kind;
            }
          }
          result = meet(suffix[first - block], prefix);
          result_missing = !NA_RM
            ? std::max(suffix_na[first - block], prefix_missing)
            : 0;
        }

        out[j * stride] = result_missing
          ? (result_missing == 2 ? NA_REAL : R_NaN)
          : result;
      }
    }

  }

private:

  // an empty window keeps the identity the from-scratch loops started from
  static double identity() {
    return IS_MIN ? R_PosInf : R_NegInf;
  }

  // walking back through a block, the incumbent is the later observation
  static double backward(double value, double incumbent) {
    if (IS_MIN)
      return value <= incumbent ? value : incumbent;
    return value > incumbent ? value : incumbent;
  }

  // walking forward, the incumbent is the earlier
  static double forward(double value, double incumbent) {
    if (IS_MIN)
      return value < incumbent ? value : incumbent;
    return value >= incumbent ? value : incumbent;
  }

  // the suffix comes from the earlier block, the prefix from the later
  static double meet(double suffix, double prefix) {
    if (IS_MIN)
      return prefix < suffix ? prefix : suffix;
    return prefix >= suffix ? prefix : suffix;
  }

  double const* x_;
  int n_;
  std::vector<double> suffix_;
  std::vector<char> suffix_na_;

};

// Strict total order over observation indices: by value, then by position.
// Distinct indices never compare equal, which is what lets the paired heaps
// below settle membership exactly -- equal values, signed zeros included, are
// told apart by where they sit in the data.
class MedianOrder {

public:

  explicit MedianOrder(double const* x) : x_(x) {}

  bool operator()(int lhs, int rhs) const {
    double a = x_[lhs];
    double b = x_[rhs];
    if (a < b) return true;
    if (b < a) return false;
    return lhs < rhs;
  }

private:

  double const* x_;

};

// the same order reversed, for the heap that wants its minimum on top
class MedianOrderReversed {

public:

  explicit MedianOrderReversed(double const* x) : order_(x) {}

  bool operator()(int lhs, int rhs) const {
    return order_(rhs, lhs);
  }

private:

  MedianOrder order_;

};

// A small window is kept in sorted order, so the median is a lookup and each
// step costs a binary search plus one memmove -- cheaper than selecting the
// middle element out of the whole window every time, and cheaper than any
// pointer structure while the memmove stays short. A large window is kept as
// a pair of heaps meeting at the median -- the lower half under a max-heap,
// the upper under a min-heap -- whose steps cost O(log n) where the memmove
// costs O(n).
//
// The heaps hold indices rather than values, ordered by value and then by
// position. Removal is then lazy: windows only ever move forward, so an
// index before the window start is dead wherever it is, and is dropped when
// it surfaces at a top. Only the live counts have to be exact, and they can
// be: an element is still live when its removal is charged, so the side it
// sits on is settled by comparing it against the lower half's live maximum
// under the strict order.
//
// The LOWER form reports the lower of an even window's two middle values
// rather than their average, which is the value a weighted median with
// uniform weights selects.
template <bool NA_RM, bool LOWER>
class MedianAccumulator :
  public WindowAccumulator< MedianAccumulator<NA_RM, LOWER> > {

  typedef WindowAccumulator< MedianAccumulator<NA_RM, LOWER> > Base;

public:

  template <typename Callable>
  MedianAccumulator(Callable, double const* x, int n)
    : Base(x, n), lower_order_(x), upper_order_(x) {

    // the memmove's bandwidth beats the heaps' pointer-chasing up to windows
    // of about two hundred observations (measured); past that the heaps win
    heaped_ = n >= 192;

    if (!heaped_ && n > 0)
      sorted_.reserve(n);

    clear();
  }

  // Sorted insertion beats re-selecting the middle even for a pair, but each
  // step pays 'by' insertions and removals of ~n/2 elements each where the
  // selection pays one pass whatever 'by' is; the measured crossover sits at
  // 'by' about a quarter of 'n', independent of 'n'.
  static bool worthwhile(int n, int by, int outputs) {
    // A handful of selections costs less than building a sorted window or
    // both heaps when there are too few slides to repay that initial work.
    return outputs > 4 && (by == 1 || n > 4LL * by);
  }

  // the window is carried in full rather than summarized, so likewise
  bool degraded() const { return false; }
  bool urgent() const { return false; }

  void prepare(int, int) {}

  void clear() {
    sorted_.clear();
    lower_.clear();
    upper_.clear();
    lower_live_ = upper_live_ = 0;
    expired_before_ = 0;
    n_na_ = 0;
  }

  void add(int i) {
    double value = this->x_[i];
    if (is_nan(value)) { ++n_na_; return; }
    if (heaped_)
      heapAdd(i);
    else
      sortedAdd(value);
  }

  void remove(int i) {
    double value = this->x_[i];
    if (is_nan(value)) { --n_na_; return; }
    if (heaped_)
      heapRemove(i);
    else
      sortedRemove(value);
  }

  double value() const {

    if (!NA_RM && n_na_)
      return NA_REAL;

    if (heaped_)
      return heapValue();

    size_t k = sorted_.size();
    if (k == 0)
      return NA_REAL;
    if (LOWER)
      return sorted_[(k - 1) / 2];
    if (k % 2 == 0)
      return midpoint(sorted_[k / 2 - 1], sorted_[k / 2]);
    return sorted_[k / 2];
  }

private:

  void sortedAdd(double value) {
    sorted_.insert(
      std::lower_bound(sorted_.begin(), sorted_.end(), value), value);
  }

  void sortedRemove(double value) {
    std::vector<double>::iterator it =
      std::lower_bound(sorted_.begin(), sorted_.end(), value);

    // -0.0 and +0.0 compare equal but are different values: erase the zero
    // whose sign matches the departing one, or the window's zeros would stop
    // reflecting the data's and a zero median could report the wrong sign
    if (value == 0.0) {
      for (std::vector<double>::iterator zit = it;
           zit != sorted_.end() && *zit == 0.0;
           ++zit) {
        if (std::signbit(*zit) == std::signbit(value)) {
          it = zit;
          break;
        }
      }
    }

    if (it != sorted_.end() && *it == value)
      sorted_.erase(it);
  }

  void heapAdd(int i) {

    // the dead cost nothing until they outnumber the living; then one sweep
    // repays what they were never popped for
    if (lower_.size() > 2 * (size_t) lower_live_ + 64)
      compactLower();
    if (upper_.size() > 2 * (size_t) upper_live_ + 64)
      compactUpper();

    purgeLower();

    if (lower_live_ == 0 || lower_order_(i, lower_.front())) {
      lower_.push_back(i);
      std::push_heap(lower_.begin(), lower_.end(), lower_order_);
      ++lower_live_;
    } else {
      upper_.push_back(i);
      std::push_heap(upper_.begin(), upper_.end(), upper_order_);
      ++upper_live_;
    }

    rebalance();
  }

  void heapRemove(int i) {

    // the element is still live here, so the boundary settles its side:
    // anything at or below the lower half's live maximum is in the lower heap
    purgeLower();
    if (lower_live_ > 0 && !lower_order_(lower_.front(), i))
      --lower_live_;
    else
      --upper_live_;

    expired_before_ = i + 1;

    rebalance();
  }

  double heapValue() const {
    int k = lower_live_ + upper_live_;
    if (k == 0)
      return NA_REAL;
    double lower_top = this->x_[lower_.front()];
    if (LOWER || k % 2 == 1)
      return lower_top;
    return midpoint(lower_top, this->x_[upper_.front()]);
  }

  // move one live top across whenever the halves drift apart; each step
  // shifts the counts by at most one, so one move restores the split. The
  // closing purges leave both tops live, which value() -- const, and so
  // unable to pop for itself -- relies on.
  void rebalance() {

    if (lower_live_ > upper_live_ + 1) {
      purgeLower();
      int index = lower_.front();
      std::pop_heap(lower_.begin(), lower_.end(), lower_order_);
      lower_.pop_back();
      upper_.push_back(index);
      std::push_heap(upper_.begin(), upper_.end(), upper_order_);
      --lower_live_;
      ++upper_live_;
    } else if (upper_live_ > lower_live_) {
      purgeUpper();
      int index = upper_.front();
      std::pop_heap(upper_.begin(), upper_.end(), upper_order_);
      upper_.pop_back();
      lower_.push_back(index);
      std::push_heap(lower_.begin(), lower_.end(), lower_order_);
      --upper_live_;
      ++lower_live_;
    }

    purgeLower();
    purgeUpper();
  }

  void purgeLower() {
    while (!lower_.empty() && lower_.front() < expired_before_) {
      std::pop_heap(lower_.begin(), lower_.end(), lower_order_);
      lower_.pop_back();
    }
  }

  void purgeUpper() {
    while (!upper_.empty() && upper_.front() < expired_before_) {
      std::pop_heap(upper_.begin(), upper_.end(), upper_order_);
      upper_.pop_back();
    }
  }

  void compactLower() {
    size_t keep = 0;
    for (size_t j = 0; j < lower_.size(); ++j)
      if (lower_[j] >= expired_before_)
        lower_[keep++] = lower_[j];
    lower_.resize(keep);
    std::make_heap(lower_.begin(), lower_.end(), lower_order_);
  }

  void compactUpper() {
    size_t keep = 0;
    for (size_t j = 0; j < upper_.size(); ++j)
      if (upper_[j] >= expired_before_)
        upper_[keep++] = upper_[j];
    upper_.resize(keep);
    std::make_heap(upper_.begin(), upper_.end(), upper_order_);
  }

  bool heaped_;

  std::vector<double> sorted_;

  MedianOrder lower_order_;
  MedianOrderReversed upper_order_;
  std::vector<int> lower_;   // max-heap of the window's lower half
  std::vector<int> upper_;   // min-heap of the window's upper half
  int lower_live_;
  int upper_live_;
  int expired_before_;       // indices before this have left the window

  int n_na_;

};

// Running products may regroup ordinary finite factors, but must not change
// whether a window overflows or underflows. Track a conservative bound on
// the sum of absolute base-two logarithms, in integer units of 1/1024. Below
// 512, every subset product stays far from either limit of a normal double.
// Other windows use the direct kernel, preserving forward multiplication.
template <bool NA_RM>
class ProdAccumulator :
  public WindowAccumulator< ProdAccumulator<NA_RM> > {

  typedef WindowAccumulator< ProdAccumulator<NA_RM> > Base;

public:

  template <typename Callable>
  ProdAccumulator(Callable, double const* x, int n)
    : Base(x, n), defer_direct_(false) {
    if (n > 0) {
      back_.reserve(n);
      suffix_.reserve(n);
    }
    clear();
  }

  static bool worthwhile(int n, int by, int) {
    return incrementalWins(n, by, NA_RM ? 52 : 96, NA_RM ? 32 : 36);
  }

  // products are never differenced, so there is no cancellation to guard;
  // whatever overflows or dies away is remade whole by the next flip
  bool degraded() const { return false; }
  bool urgent() const { return false; }

  void prepare(int, int) {}

  void clear() {
    back_.clear();
    suffix_.clear();
    back_product_ = 1.0;
    n_na_ = n_nan_ = 0;
    risk_ = 0;
  }

  void add(int i) {
    double value = this->x_[i];
    if (is_nan(value)) { if (ISNA(value)) ++n_na_; else ++n_nan_; return; }
    risk_ += riskUnits(value);
    back_.push_back(value);
    back_product_ *= value;
  }

  void remove(int i) {
    double value = this->x_[i];
    if (is_nan(value)) { if (ISNA(value)) --n_na_; else --n_nan_; return; }
    risk_ -= riskUnits(value);

    // out of suffixes: flip the back stack into suffix products, oldest on
    // top, so that this and the following removals are single pops
    if (suffix_.empty()) {
      size_t count = back_.size();
      suffix_.resize(count);
      double product = 1.0;
      for (size_t j = 0; j < count; ++j) {
        product *= back_[count - 1 - j];
        suffix_[j] = product;
      }
      back_.clear();
      back_product_ = 1.0;
    }

    suffix_.pop_back();
  }

  double value() const {

    // as for the running total: NA and NaN are counted apart so that each
    // keeps its identity, with NA reported where multiplication order used
    // to decide
    if (!NA_RM && (n_na_ || n_nan_))
      return n_na_ ? NA_REAL : R_NaN;

    if (needsDirect())
      return defer_direct_ ? 0.0 : direct_(
        this->x_, this->start_, this->end_ - this->start_ + 1);

    double front = suffix_.empty() ? 1.0 : suffix_.back();
    return front * back_product_;
  }

  void computeStrip(int start, int by, int count, double* out, int stride) {
    // Batch consecutive exceptional windows into SIMD strips as well,
    // avoiding a separate scalar calculation for each risky window.
    defer_direct_ = true;
    int pending = -1;
    for (int j = 0; j < count; ++j) {
      int first = start + j * by;
      double result = this->compute(first, first + this->n_ - 1);
      if (needsDirect()) {
        if (pending < 0) pending = j;
      } else {
        if (pending >= 0) {
          direct_.strip(this->x_, start + pending * by, by, this->n_,
                        (double const*) NULL, j - pending,
                        out + pending * stride, stride);
          pending = -1;
        }
        out[j * stride] = result;
      }
    }
    if (pending >= 0)
      direct_.strip(this->x_, start + pending * by, by, this->n_,
                    (double const*) NULL, count - pending,
                    out + pending * stride, stride);
    defer_direct_ = false;
  }

private:

  static int riskUnits(double value) {
    double magnitude = fabs(value);
    if (magnitude == 0.0 || !is_finite(magnitude))
      return 1024 * 1024;
    double distance = fabs(magnitude - 1.0);
    // On [0.5, 1.5], |log2(x)| <= 4 * |x - 1|. This avoids a
    // transcendental call for returns and other factors close to one.
    if (distance <= 0.5)
      return (int) std::ceil(4096.0 * distance);
    int exponent;
    std::frexp(magnitude, &exponent);
    return 1024 * (magnitude > 1.0 ? exponent : 1 - exponent);
  }

  bool needsDirect() const {
    return risk_ >= 512 * 1024 && (NA_RM || !(n_na_ || n_nan_));
  }

  prod_f<NA_RM> direct_;
  long long risk_;
  bool defer_direct_;
  std::vector<double> back_;    // values since the last flip, oldest first
  std::vector<double> suffix_;  // suffix products, the oldest value's on top
  double back_product_;
  int n_na_;
  int n_nan_;

};

// The operation a run of uniform weights reduces to, once 'normalize' has
// taken them to exactly one: usually the operation itself. The median is the
// exception -- its weighted form selects the lower of an even window's two
// middle values where the unweighted form averages them, so it reduces to its
// lower form rather than to the plain median.
template <typename Callable>
inline Callable uniform_equivalent(Callable f) {
  return f;
}

template <bool NA_RM>
inline median_f<NA_RM, true> uniform_equivalent(median_f<NA_RM, false>) {
  return median_f<NA_RM, true>();
}

// Which incremental accumulator stands in for a given windowing function.
// Anything without one falls back to recomputing the window.
template <typename Callable>
struct accumulator_for {
  typedef DirectAccumulator<Callable> type;
};

template <bool NA_RM>
struct accumulator_for< mean_f<NA_RM> > {
  typedef SumAccumulator<NA_RM, true> type;
};

template <bool NA_RM>
struct accumulator_for< sum_f<NA_RM> > {
  typedef SumAccumulator<NA_RM, false> type;
};

template <bool NA_RM>
struct accumulator_for< min_f<NA_RM> > {
  typedef ExtremumAccumulator<NA_RM, true> type;
};

template <bool NA_RM>
struct accumulator_for< max_f<NA_RM> > {
  typedef ExtremumAccumulator<NA_RM, false> type;
};

template <bool NA_RM>
struct accumulator_for< prod_f<NA_RM> > {
  typedef ProdAccumulator<NA_RM> type;
};

template <bool NA_RM>
struct accumulator_for< var_f<NA_RM> > {
  typedef VarAccumulator<NA_RM, false> type;
};

template <bool NA_RM>
struct accumulator_for< sd_f<NA_RM> > {
  typedef VarAccumulator<NA_RM, true> type;
};

template <bool NA_RM, bool LOWER>
struct accumulator_for< median_f<NA_RM, LOWER> > {
  typedef MedianAccumulator<NA_RM, LOWER> type;
};

// ---------------------------------------------------------------------------
// Chunking and threads
//
// The drivers below walk their windows a chunk at a time, each chunk starting
// from a fresh copy of a prototype accumulator. Chunks share nothing, and the
// window loops touch no R API, so where OpenMP support was compiled in the
// chunks run across threads. A serial build walks the very same chunks in
// order, so results never depend on the thread count -- nor on whether there
// are threads at all.
// ---------------------------------------------------------------------------

// Chunk size, in windows. Restarting an accumulator costs one window's worth
// of adds, so chunks scale with the window to keep that amortized away, with
// a floor so that a chunk is never trivial. Deliberately a function of the
// window alone: thread counts must not move chunk boundaries, or results
// would depend on them.
inline int chunkSize(int width) {
  long long chunk = 8192;
  if (8LL * width > chunk)
    chunk = 8LL * width;
  if (chunk > (1 << 30))
    chunk = (1 << 30);
  return (int) chunk;
}

#ifdef _OPENMP

// set in init.c when this process is a forked child; see the note there
extern "C" {
extern int rcpproll_forked;
}

// The thread count requested through options(RcppRoll.threads = <n>). A
// missing option defers to the OpenMP runtime default, which itself respects
// e.g. OMP_NUM_THREADS. Reads an R option, so this must stay on the main
// thread, outside any parallel region.
inline int requestedThreads() {
  SEXP option = Rf_GetOption1(Rf_install("RcppRoll.threads"));
  if (option != R_NilValue) {
    if ((TYPEOF(option) != INTSXP && TYPEOF(option) != REALSXP) ||
        Rf_isObject(option) || Rf_xlength(option) != 1)
      Rf_error("option 'RcppRoll.threads' should be a positive integer scalar");

    double requested = TYPEOF(option) == INTSXP
      ? (double) INTEGER(option)[0]
      : REAL(option)[0];
    if (!is_finite(requested) || requested < 1.0 || requested > INT_MAX ||
        requested != floor(requested))
      Rf_error("option 'RcppRoll.threads' should be a positive integer scalar");
    return (int) requested;
  }
  return omp_get_max_threads();
}

#endif

// The number of threads one roll_*() call may put to work, read once on the
// main thread before any window is walked: consulting the option touches the
// R API, and the column loop below may run everything downstream of it on
// worker threads, where no R API call may fire. A serial build's budget is
// one.
inline int threadBudget() {
#ifdef _OPENMP
  if (rcpproll_forked)
    return 1;
  return requestedThreads();
#else
  return 1;
#endif
}

inline int threadCount(int chunks, int budget) {
  if (chunks < 2)
    return 1;
  return budget < chunks ? budget : chunks;
}

// ---------------------------------------------------------------------------
// Drivers
//
// Each writes 'rollOutputSize()' values into a buffer the caller owns, so that
// the matrix routine can hand over a column of its output directly.
// ---------------------------------------------------------------------------

// One window reported at 'i', clipped to the data.
template <typename Accumulator>
inline double roll_clipped(Accumulator& accumulator,
                           int x_n,
                           int i,
                           int leftOffset,
                           int rightOffset) {
  int start = i - leftOffset;
  int stop = rightOffset > x_n - 1 - i ? x_n - 1 : i + rightOffset;
  if (start < 0) start = 0;
  return accumulator.compute(start, stop);
}

// Walk the clipped windows, writing one value per point. The windows clipped
// at neither edge form one run, handed over a chunk at a time as strips.
template <typename Accumulator>
void roll_partial_windows(Accumulator const& prototype,
                          int x_n,
                          double* output,
                          int width,
                          int by,
                          int leftOffset,
                          int rightOffset,
                          int threads) {

  int ops = x_n ? (x_n - 1) / by + 1 : 0;
  int chunk = chunkSize(width);
  int chunks = ops ? (ops - 1) / chunk + 1 : 0;

  // the windows reported at least 'leftOffset' from the start and
  // 'rightOffset' from the end are whole: those from 'whole_from' up to
  // 'whole_to'. A window wider than the data leaves none, bar the case of a
  // single observation, whose window has the accumulator's (clipped) width.
  int whole_from = leftOffset / by + (leftOffset % by != 0);
  int whole_to = x_n - 1 - rightOffset >= 0
    ? (x_n - 1 - rightOffset) / by + 1
    : 0;

#ifdef _OPENMP
  int team = threadCount(chunks, threads);
# pragma omp parallel for num_threads(team) if (team > 1)
#endif
  for (int c = 0; c < chunks; ++c) {
    Accumulator accumulator(prototype);
    int begin = c * chunk;
    int end = ops - begin > chunk ? begin + chunk : ops;

    int j = begin;
    for (; j < end && j < whole_from; ++j)
      output[j * by] =
        roll_clipped(accumulator, x_n, j * by, leftOffset, rightOffset);

    if (j < end && j < whole_to) {
      int to = end < whole_to ? end : whole_to;
      accumulator.computeStrip(
        j * by - leftOffset, by, to - j, output + j * by, by);
      j = to;
    }

    for (; j < end; ++j)
      output[j * by] =
        roll_clipped(accumulator, x_n, j * by, leftOffset, rightOffset);
  }

}

// Windows clipped to the bounds of 'x': every point gets an answer, computed
// over however many observations are in range. The point a window is reported
// at is always in range itself, so a window is never empty.
template <typename Callable>
void roll_vector_partial_into(Callable f,
                              double const* x,
                              int x_n,
                              double* output,
                              int n,
                              int by,
                              char const* align,
                              int threads) {

  int leftOffset  = getLeftOffset(align, n);
  int rightOffset = getRightOffset(align, n);

  // a window wider than the data is clipped to it on both sides, so cap what
  // a window can reach -- and, below, what an accumulator reserves for one --
  // rather than letting an outsized 'n' drive either
  if (leftOffset > x_n - 1) leftOffset = x_n - 1;
  if (rightOffset > x_n - 1) rightOffset = x_n - 1;

  // points we skip over are not computed, and 'fill' does not apply here
  if (by != 1)
    std::fill(output, output + x_n, NA_REAL);

  // the cap applies to what an accumulator reserves for a window, too
  int width = n < x_n ? n : x_n;

  typedef typename accumulator_for<Callable>::type Incremental;
  int outputs = x_n ? (x_n - 1) / by + 1 : 0;
  if (Incremental::worthwhile(n, by, outputs))
    roll_partial_windows(
      Incremental(f, x, width), x_n, output, width, by,
      leftOffset, rightOffset, threads);
  else
    roll_partial_windows(
      DirectAccumulator<Callable>(f, x, width), x_n, output, width, by,
      leftOffset, rightOffset, threads);

}

// Walk the whole windows, writing 'output' from 'from' up to 'to'. Returns one
// step past the last window computed, which is where the right-hand fill picks
// up.
template <typename Accumulator>
int roll_fill_windows(Accumulator const& prototype,
                      double* output,
                      int n,
                      int by,
                      int from,
                      int to,
                      int padLeftTimes,
                      int threads) {

  int ops = to > from ? (to - from - 1) / by + 1 : 0;
  int chunk = chunkSize(n);
  int chunks = ops ? (ops - 1) / chunk + 1 : 0;

#ifdef _OPENMP
  int team = threadCount(chunks, threads);
# pragma omp parallel for num_threads(team) if (team > 1)
#endif
  for (int c = 0; c < chunks; ++c) {
    Accumulator accumulator(prototype);
    int begin = c * chunk;
    int end = ops - begin > chunk ? begin + chunk : ops;
    int i = from + begin * by;
    accumulator.computeStrip(i - padLeftTimes, by, end - begin, output + i, by);
  }

  return ops
    ? (int) ((long long) from + (long long) (ops - 1) * by + 1)
    : from;
}

template <typename Callable>
void roll_vector_fill_into(Callable f,
                           double const* x,
                           int x_n,
                           double* output,
                           int n,
                           double const* weights,
                           int weights_n,
                           int by,
                           Fill const& fill,
                           char const* align,
                           int threads) {

  if (x_n < n) {
    std::fill(output, output + x_n, fill.left());
    return;
  }

  // figure out if we need to pad at the start, end, etc.
  int padLeftTimes  = getLeftPadding(fill, align, n);
  int padRightTimes = getRightPadding(fill, align, n);

  int ops_n = x_n - n + 1;
  int output_n = padLeftTimes + ops_n + padRightTimes;

  // points we step over keep the middle fill
  if (by != 1)
    std::fill(output, output + output_n, fill.middle());

  int i = 0;

  // Pad left
  for (; i < padLeftTimes; ++i)
    output[i] = fill.left();

  // Fill result -- the driver reports one step past the last window it
  // computed, which is where the 'fill-right' pass-through should start
  int to = padLeftTimes + ops_n;
  if (weights_n) {
    i = roll_fill_windows(
      WeightedAccumulator<Callable>(f, x, weights, n),
      output, n, by, i, to, padLeftTimes, threads);
  } else {
    typedef typename accumulator_for<Callable>::type Incremental;
    int outputs = (ops_n - 1) / by + 1;
    i = Incremental::worthwhile(n, by, outputs) ?
      roll_fill_windows(
        Incremental(f, x, n), output, n, by, i, to, padLeftTimes, threads) :
      roll_fill_windows(
        DirectAccumulator<Callable>(f, x, n), output, n, by, i, to,
        padLeftTimes, threads);
  }

  // Fill-right on the remainders after the last computed window.
  for (; i < output_n; ++i)
    output[i] = fill.right();

}

template <typename Accumulator>
void roll_nofill_windows(Accumulator const& prototype,
                         double* output,
                         int n,
                         int by,
                         int output_n,
                         int threads) {

  int chunk = chunkSize(n);
  int chunks = output_n ? (output_n - 1) / chunk + 1 : 0;

#ifdef _OPENMP
  int team = threadCount(chunks, threads);
# pragma omp parallel for num_threads(team) if (team > 1)
#endif
  for (int c = 0; c < chunks; ++c) {
    Accumulator accumulator(prototype);
    int begin = c * chunk;
    int end = output_n - begin > chunk ? begin + chunk : output_n;
    accumulator.computeStrip(begin * by, by, end - begin, output + begin, 1);
  }
}

template <typename Callable>
void roll_vector_nofill_into(Callable f,
                             double const* x,
                             int x_n,
                             double* output,
                             int n,
                             double const* weights,
                             int weights_n,
                             int by,
                             int threads) {

  // no complete windows fit, and the output was sized accordingly
  if (x_n < n)
    return;

  int output_n = (x_n - n) / by + 1;

  if (weights_n) {
    roll_nofill_windows(
      WeightedAccumulator<Callable>(f, x, weights, n), output, n, by,
      output_n, threads);
  } else {
    typedef typename accumulator_for<Callable>::type Incremental;
    if (Incremental::worthwhile(n, by, output_n))
      roll_nofill_windows(
        Incremental(f, x, n), output, n, by, output_n, threads);
    else
      roll_nofill_windows(
        DirectAccumulator<Callable>(f, x, n), output, n, by, output_n,
        threads);
  }

}

template <typename Callable>
void roll_vector_into(Callable f,
                      double const* x,
                      int x_n,
                      double* output,
                      int n,
                      double const* weights,
                      int weights_n,
                      int by,
                      Fill const& fill,
                      bool partial,
                      char const* align,
                      int threads) {

  // partial windows are computable at every point, so there is nothing to
  // shorten or to pad; 'weights' is rejected upstream in this case
  if (partial)
    roll_vector_partial_into(f, x, x_n, output, n, by, align, threads);
  else if (fill.filled())
    roll_vector_fill_into(
      f, x, x_n, output, n, weights, weights_n, by, fill, align, threads);
  else
    roll_vector_nofill_into(
      f, x, x_n, output, n, weights, weights_n, by, threads);

}

template <typename Callable>
SEXP roll_vector_with(Callable f,
                      SEXP data,
                      int n,
                      double const* weights,
                      int weights_n,
                      int by,
                      Fill const& fill,
                      bool partial,
                      char const* align,
                      int threads) {

  SEXP x = PROTECT(Rf_coerceVector(data, REALSXP));
  int x_n = Rf_length(x);

  SEXP output =
    PROTECT(Rf_allocVector(REALSXP, rollOutputSize(x_n, n, by, fill, partial)));

  roll_vector_into(
    f, REAL(x), x_n, REAL(output),
    n, weights, weights_n, by, fill, partial, align, threads);

  UNPROTECT(2);
  return output;
}

// A matrix keeps its data column-major and contiguous, so each column can be
// handed over as a pointer and its results written straight into the output
// -- no per-column allocation, and no copying in or out.
template <typename Callable>
SEXP roll_matrix_with(Callable f,
                      SEXP data,
                      int n,
                      double const* weights,
                      int weights_n,
                      int by,
                      Fill const& fill,
                      bool partial,
                      char const* align,
                      int threads) {

  int nrow = Rf_nrows(data);
  int ncol = Rf_ncols(data);
  int output_nrow = rollOutputSize(nrow, n, by, fill, partial);

  // the offsets are recomputed inside each column's walk, possibly on a
  // worker thread, where an Rf_error() must never fire -- validate here
  getLeftOffset(align, n);

  SEXP x = PROTECT(Rf_coerceVector(data, REALSXP));
  SEXP output = PROTECT(Rf_allocMatrix(REALSXP, output_nrow, ncol));

  // the input's column names still apply; its row names cannot, since the
  // output usually has a different number of rows
  SEXP dimnames = Rf_getAttrib(data, R_DimNamesSymbol);
  if (dimnames != R_NilValue && VECTOR_ELT(dimnames, 1) != R_NilValue) {
    SEXP outputNames = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(outputNames, 0, R_NilValue);
    SET_VECTOR_ELT(outputNames, 1, VECTOR_ELT(dimnames, 1));
    Rf_setAttrib(output, R_DimNamesSymbol, outputNames);
    UNPROTECT(1);
  }

  double const* source = REAL(x);
  double* target = REAL(output);

  // Short columns offer the drivers fewer chunks than the matrix offers
  // columns, so the parallelism moves up a level: the columns run across
  // threads, each column walked serially. The count mirrors the chunking the
  // drivers would use, and the choice moves no chunk boundary, so results
  // stay identical either way -- columns share nothing, and everything a
  // column touches from here down is R-API-free.
  int column_threads = 1;
#ifdef _OPENMP
  if (threads > 1 && ncol > 1) {

    int width = n;
    if (partial && nrow < n)
      width = nrow;

    int ops = partial
      ? (nrow ? (nrow - 1) / by + 1 : 0)
      : (nrow >= n ? (nrow - n) / by + 1 : 0);

    int column_chunks = ops ? (ops - 1) / chunkSize(width) + 1 : 0;
    if (column_chunks <= ncol)
      column_threads = threads < ncol ? threads : ncol;
  }
#endif

  int worker_threads = column_threads > 1 ? 1 : threads;

#ifdef _OPENMP
# pragma omp parallel for num_threads(column_threads) if (column_threads > 1)
#endif
  for (int j = 0; j < ncol; ++j) {
    roll_vector_into(f, source + (R_xlen_t) j * nrow, nrow,
                     target + (R_xlen_t) j * output_nrow,
                     n, weights, weights_n, by,
                     fill, partial, align, worker_threads);
  }

  UNPROTECT(2);
  return output;
}

template <typename Callable>
SEXP roll_dispatch(Callable f,
                   SEXP data,
                   int n,
                   double const* weights,
                   int weights_n,
                   int by,
                   Fill const& fill,
                   bool partial,
                   char const* align,
                   int threads) {

  if (Rf_isMatrix(data))
    return roll_matrix_with(
      f, data, n, weights, weights_n, by, fill, partial, align, threads);

  return roll_vector_with(
    f, data, n, weights, weights_n, by, fill, partial, align, threads);

}

// Shared entry point for the generated exports below: 'weights' settles the
// window size, and is normalized once here rather than once per column.
template <typename Callable>
SEXP roll_with(Callable f,
               SEXP data,
               int n,
               SEXP weights,
               int by,
               Fill const& fill,
               bool partial,
               char const* align,
               bool normalize) {

  if (!Rf_isNumeric(data))
    Rf_error("'x' should be a numeric vector or matrix");

  // a higher-dimensional array is almost certainly a mistake: rolling over
  // its flattened data would silently cross slice boundaries
  SEXP dims = Rf_getAttrib(data, R_DimSymbol);
  if (dims != R_NilValue && Rf_length(dims) > 2)
    Rf_error("'x' should be a numeric vector or matrix");

  // Normalize 'n' to match that of weights
  int weights_n = Rf_length(weights);
  if (weights_n)
    n = weights_n;

  // the R wrappers reject these already; this backstop keeps a direct .Call
  // from sizing an output the window loops would then write past
  if (n < 1)
    Rf_error("'n' should be a positive integer");
  if (by < 1)
    Rf_error("'by' should be a positive integer");

  // Validate before normalized weights allocate. The same offset helpers may
  // run inside an OpenMP worker after dispatch, where the R API is forbidden.
  getLeftOffset(align, n);

  double const* raw_weights = weights_n ? REAL(weights) : NULL;
  validate_weights(f, raw_weights, weights_n);
  f.setNormalize(normalize);

  // read once, up front: the option is R state, and the walks below may
  // leave the main thread
  int threads = threadBudget();

  // uniform weights are an unweighted call in disguise, so route them to the
  // unweighted loops, which carry their windows incrementally where the
  // weighted forms recompute every window
  if (weightsAreUniform(raw_weights, weights_n, normalize))
    return roll_dispatch(
      uniform_equivalent(f), data, n,
      (double const*) NULL, 0, by, fill, partial, align, threads);

  std::vector<double> scaled =
    normalizeWeights(raw_weights, weights_n, n, normalize);
  double const* weights_data = scaled.empty() ? NULL : &scaled[0];

  return roll_dispatch(
    f, data, n, weights_data, weights_n, by, fill, partial, align, threads);

}

}  // end namespace RcppRoll

extern "C" SEXP na_locf(SEXP x)
{
  // Factors are integer vectors with level/class attributes. Coercing their
  // storage to double while retaining those attributes creates a malformed
  // factor, so carry the integer codes forward in place on a duplicate.
  if (Rf_inherits(x, "factor"))
  {
    if (TYPEOF(x) != INTSXP)
      Rf_error("malformed factor");

    SEXP output = PROTECT(Rf_duplicate(x));
    int* data = INTEGER(output);
    R_xlen_t n = Rf_xlength(output);
    int lastNonNA = NA_INTEGER;
    for (R_xlen_t i = 0; i < n; ++i)
    {
      int value = data[i];
      if (value != NA_INTEGER)
        lastNonNA = value;
      else
        data[i] = lastNonNA;
    }
    UNPROTECT(1);
    return output;
  }

  // a double vector with nothing missing is its own answer -- return it
  // rather than copying it
  if (TYPEOF(x) == REALSXP)
  {
    double const* data = REAL(x);
    R_xlen_t n = Rf_xlength(x);

    R_xlen_t i = 0;
    while (i < n && !RcppRoll::is_nan(data[i]))
      ++i;

    if (i == n)
      return x;
  }

  SEXP output = PROTECT(
    TYPEOF(x) == REALSXP ? Rf_duplicate(x) : Rf_coerceVector(x, REALSXP));

  double* data = REAL(output);
  R_xlen_t n = Rf_xlength(output);

  double lastNonNA = NA_REAL;
  for (R_xlen_t i = 0; i < n; ++i)
  {
    double value = data[i];
    if (!RcppRoll::is_nan(value))
      lastNonNA = value;
    else
      data[i] = lastNonNA;
  }

  UNPROTECT(1);
  return output;
}

// The number of threads the window drivers would put to work on a large
// enough input, or NA when the package was compiled without OpenMP support.
// Exposed as roll_threads(), so users can check what a source build ended
// up with.
extern "C" SEXP roll_threads_impl(void)
{
#ifdef _OPENMP
  int threads = RcppRoll::rcpproll_forked ? 1 : RcppRoll::requestedThreads();
  return Rf_ScalarInteger(threads);
#else
  return Rf_ScalarInteger(NA_INTEGER);
#endif
}

// Begin auto-generated exports (internal/make-exports.R)

extern "C" SEXP roll_mean_impl(SEXP x,
                             SEXP n,
                             SEXP weights,
                             SEXP by,
                             SEXP fill,
                             SEXP partial,
                             SEXP align,
                             SEXP normalize,
                             SEXP na_rm)
{
  RcppRoll::Fill fill_(fill);
  if (Rf_asLogical(na_rm)) {
    return RcppRoll::roll_with(
      RcppRoll::mean_f<true>(), x, Rf_asInteger(n), weights, Rf_asInteger(by),
      fill_, Rf_asLogical(partial), CHAR(STRING_ELT(align, 0)),
      Rf_asLogical(normalize));
  } else {
    return RcppRoll::roll_with(
      RcppRoll::mean_f<false>(), x, Rf_asInteger(n), weights, Rf_asInteger(by),
      fill_, Rf_asLogical(partial), CHAR(STRING_ELT(align, 0)),
      Rf_asLogical(normalize));
  }
}
extern "C" SEXP roll_median_impl(SEXP x,
                             SEXP n,
                             SEXP weights,
                             SEXP by,
                             SEXP fill,
                             SEXP partial,
                             SEXP align,
                             SEXP normalize,
                             SEXP na_rm)
{
  RcppRoll::Fill fill_(fill);
  if (Rf_asLogical(na_rm)) {
    return RcppRoll::roll_with(
      RcppRoll::median_f<true>(), x, Rf_asInteger(n), weights, Rf_asInteger(by),
      fill_, Rf_asLogical(partial), CHAR(STRING_ELT(align, 0)),
      Rf_asLogical(normalize));
  } else {
    return RcppRoll::roll_with(
      RcppRoll::median_f<false>(), x, Rf_asInteger(n), weights, Rf_asInteger(by),
      fill_, Rf_asLogical(partial), CHAR(STRING_ELT(align, 0)),
      Rf_asLogical(normalize));
  }
}
extern "C" SEXP roll_min_impl(SEXP x,
                             SEXP n,
                             SEXP weights,
                             SEXP by,
                             SEXP fill,
                             SEXP partial,
                             SEXP align,
                             SEXP normalize,
                             SEXP na_rm)
{
  RcppRoll::Fill fill_(fill);
  if (Rf_asLogical(na_rm)) {
    return RcppRoll::roll_with(
      RcppRoll::min_f<true>(), x, Rf_asInteger(n), weights, Rf_asInteger(by),
      fill_, Rf_asLogical(partial), CHAR(STRING_ELT(align, 0)),
      Rf_asLogical(normalize));
  } else {
    return RcppRoll::roll_with(
      RcppRoll::min_f<false>(), x, Rf_asInteger(n), weights, Rf_asInteger(by),
      fill_, Rf_asLogical(partial), CHAR(STRING_ELT(align, 0)),
      Rf_asLogical(normalize));
  }
}
extern "C" SEXP roll_max_impl(SEXP x,
                             SEXP n,
                             SEXP weights,
                             SEXP by,
                             SEXP fill,
                             SEXP partial,
                             SEXP align,
                             SEXP normalize,
                             SEXP na_rm)
{
  RcppRoll::Fill fill_(fill);
  if (Rf_asLogical(na_rm)) {
    return RcppRoll::roll_with(
      RcppRoll::max_f<true>(), x, Rf_asInteger(n), weights, Rf_asInteger(by),
      fill_, Rf_asLogical(partial), CHAR(STRING_ELT(align, 0)),
      Rf_asLogical(normalize));
  } else {
    return RcppRoll::roll_with(
      RcppRoll::max_f<false>(), x, Rf_asInteger(n), weights, Rf_asInteger(by),
      fill_, Rf_asLogical(partial), CHAR(STRING_ELT(align, 0)),
      Rf_asLogical(normalize));
  }
}
extern "C" SEXP roll_prod_impl(SEXP x,
                             SEXP n,
                             SEXP weights,
                             SEXP by,
                             SEXP fill,
                             SEXP partial,
                             SEXP align,
                             SEXP normalize,
                             SEXP na_rm)
{
  RcppRoll::Fill fill_(fill);
  if (Rf_asLogical(na_rm)) {
    return RcppRoll::roll_with(
      RcppRoll::prod_f<true>(), x, Rf_asInteger(n), weights, Rf_asInteger(by),
      fill_, Rf_asLogical(partial), CHAR(STRING_ELT(align, 0)),
      Rf_asLogical(normalize));
  } else {
    return RcppRoll::roll_with(
      RcppRoll::prod_f<false>(), x, Rf_asInteger(n), weights, Rf_asInteger(by),
      fill_, Rf_asLogical(partial), CHAR(STRING_ELT(align, 0)),
      Rf_asLogical(normalize));
  }
}
extern "C" SEXP roll_sum_impl(SEXP x,
                             SEXP n,
                             SEXP weights,
                             SEXP by,
                             SEXP fill,
                             SEXP partial,
                             SEXP align,
                             SEXP normalize,
                             SEXP na_rm)
{
  RcppRoll::Fill fill_(fill);
  if (Rf_asLogical(na_rm)) {
    return RcppRoll::roll_with(
      RcppRoll::sum_f<true>(), x, Rf_asInteger(n), weights, Rf_asInteger(by),
      fill_, Rf_asLogical(partial), CHAR(STRING_ELT(align, 0)),
      Rf_asLogical(normalize));
  } else {
    return RcppRoll::roll_with(
      RcppRoll::sum_f<false>(), x, Rf_asInteger(n), weights, Rf_asInteger(by),
      fill_, Rf_asLogical(partial), CHAR(STRING_ELT(align, 0)),
      Rf_asLogical(normalize));
  }
}
extern "C" SEXP roll_sd_impl(SEXP x,
                             SEXP n,
                             SEXP weights,
                             SEXP by,
                             SEXP fill,
                             SEXP partial,
                             SEXP align,
                             SEXP normalize,
                             SEXP na_rm)
{
  RcppRoll::Fill fill_(fill);
  if (Rf_asLogical(na_rm)) {
    return RcppRoll::roll_with(
      RcppRoll::sd_f<true>(), x, Rf_asInteger(n), weights, Rf_asInteger(by),
      fill_, Rf_asLogical(partial), CHAR(STRING_ELT(align, 0)),
      Rf_asLogical(normalize));
  } else {
    return RcppRoll::roll_with(
      RcppRoll::sd_f<false>(), x, Rf_asInteger(n), weights, Rf_asInteger(by),
      fill_, Rf_asLogical(partial), CHAR(STRING_ELT(align, 0)),
      Rf_asLogical(normalize));
  }
}
extern "C" SEXP roll_var_impl(SEXP x,
                             SEXP n,
                             SEXP weights,
                             SEXP by,
                             SEXP fill,
                             SEXP partial,
                             SEXP align,
                             SEXP normalize,
                             SEXP na_rm)
{
  RcppRoll::Fill fill_(fill);
  if (Rf_asLogical(na_rm)) {
    return RcppRoll::roll_with(
      RcppRoll::var_f<true>(), x, Rf_asInteger(n), weights, Rf_asInteger(by),
      fill_, Rf_asLogical(partial), CHAR(STRING_ELT(align, 0)),
      Rf_asLogical(normalize));
  } else {
    return RcppRoll::roll_with(
      RcppRoll::var_f<false>(), x, Rf_asInteger(n), weights, Rf_asInteger(by),
      fill_, Rf_asLogical(partial), CHAR(STRING_ELT(align, 0)),
      Rf_asLogical(normalize));
  }
}
// End auto-generated exports (internal/make-exports.R)
