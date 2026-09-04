#include <Rcpp.h>

#include <algorithm>
#include <deque>
#include <utility>
#include <vector>

using namespace Rcpp;

namespace RcppRoll {

class Fill {

public:

Fill (NumericVector const& vector) {
  switch (Rf_length(vector)) {
    case 0: {
      filled_ = false;
      break;
    }
    case 1: {
      left_ = middle_ = right_ = vector[0];
      filled_ = true;
      break;
    }
    case 3: {
      left_ = vector[0];
      middle_ = vector[1];
      right_ = vector[2];
      filled_ = true;
      break;
    }
    default: {
      stop("'fill' should be a vector of size 0, 1, or 3");
    }
  }
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
inline int getLeftOffset(String const& align, int n) {
  if (align == "left") {
    return 0;
  } else if (align == "center") {
    return (n - 1) / 2; // round down
  } else if (align == "right") {
    return n - 1;
  } else {
    stop("Invalid 'align'");
  }
  return -1; // silence compiler
}

inline int getRightOffset(String const& align, int n) {
  if (align == "left") {
    return n - 1;
  } else if (align == "center") {
    return n / 2;
  } else if (align == "right") {
    return 0;
  } else {
    stop("Invalid 'align'");
  }
  return -1; // silence compiler
}

inline int getLeftPadding(Fill const& fill, String const& align, int n) {
  if (!fill.filled()) return 0;
  return getLeftOffset(align, n);
}

inline int getRightPadding(Fill const& fill, String const& align, int n) {
  if (!fill.filled()) return 0;
  return getRightOffset(align, n);
}

// How many values the vector routines produce. The padding either side of the
// whole windows always comes to 'n - 1' elements, so 'fill' -- like 'partial'
// -- gives one output per input; otherwise only whole windows are reported.
inline int rollOutputSize(int x_n, int n, int by, Fill const& fill, bool partial) {
  if (partial || fill.filled())
    return x_n;
  return (x_n - n) / by + 1;
}

// 'normalize' rescales the weights so that they sum to 'n'. Done once here
// rather than once per column of a matrix, and without touching the caller's
// vector.
inline NumericVector normalizeWeights(NumericVector const& weights,
                                      int n,
                                      bool normalize) {
  if (!normalize || !weights.size())
    return weights;
  return NumericVector(weights / sum(weights) * n);
}

// sqrt() would turn NA_REAL into a plain NaN, so pass non-values through
inline double window_sqrt(double value) {
  return ISNAN(value) ? value : sqrt(value);
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
    if (R_FINITE(updated)) {
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
    if (!R_FINITE(result))
      return true;
    return magnitude_ > 1e12 * fabs(result);
  }

private:

  double total_;
  double compensation_;
  double magnitude_;

};

// ---------------------------------------------------------------------------
// Windowed accumulators
//
// Computing each window from scratch costs O(n) per point. The accumulators
// below instead carry the state of one window forward, paying only for the
// observations that enter and leave it. That works because every sequence of
// windows generated here moves monotonically: neither edge ever steps
// backwards, whatever 'by', 'align' and 'partial' are set to.
// ---------------------------------------------------------------------------

// Drives one accumulator over a sequence of windows. Derived classes supply
// clear() / add() / remove() / value(); this is a compile-time (CRTP) base, so
// none of that costs a virtual call in the inner loop.
template <typename Derived>
class WindowAccumulator {

public:

  WindowAccumulator(NumericVector const& x)
    : x_(x), start_(0), end_(-1) {}

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
    }

    while (end_ < end) {
      ++end_;
      self.add(end_);
    }

    // Sliding a window subtracts values that were added earlier, which no
    // amount of care makes safe when the window spans magnitudes far enough
    // apart. Where that has happened, fall back to reading the window itself.
    if (start_ <= end_ && self.degraded()) {
      self.clear();
      self.prepare(start_, end_);
      for (int i = start_; i <= end_; ++i)
        self.add(i);
    }

    return self.value();
  }

protected:

  NumericVector const& x_;
  int start_;
  int end_;

};

// Fallback for operations with no incremental form -- recompute the window.
template <typename Callable>
class DirectAccumulator {

public:

  DirectAccumulator(Callable f, NumericVector const& x, int /* n */)
    : f_(f), x_(x) {}

  // an operation with no incremental form always reads the window afresh
  static bool worthwhile(int, int) { return false; }

  double compute(int start, int end) {
    return f_(x_, start, end - start + 1);
  }

private:

  Callable f_;
  NumericVector const& x_;

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
  SumAccumulator(Callable, NumericVector const& x, int) : Base(x) {
    clear();
  }

  // One add and one subtract per observation entering or leaving, against a
  // window the compiler can vectorize. Each step slides the window 'by'
  // observations, so the crossover scales with 'by' -- and a 'by' past the
  // crossover includes every 'by' wide enough to leave gaps, where there is
  // nothing to carry forward at all.
  static bool worthwhile(int n, int by) { return n >= 48LL * by; }

  bool degraded() const { return total_.degraded(); }

  void prepare(int, int) {}

  void clear() {
    total_.clear();
    n_finite_ = n_na_ = n_pos_inf_ = n_neg_inf_ = 0;
  }

  void add(int i) {
    double value = this->x_[i];
    if (ISNAN(value)) ++n_na_;
    else if (value == R_PosInf) ++n_pos_inf_;
    else if (value == R_NegInf) ++n_neg_inf_;
    else { total_.add(value); ++n_finite_; }
  }

  void remove(int i) {
    double value = this->x_[i];
    if (ISNAN(value)) --n_na_;
    else if (value == R_PosInf) --n_pos_inf_;
    else if (value == R_NegInf) --n_neg_inf_;
    else { total_.remove(value); --n_finite_; }
  }

  double value() const {

    if (!NA_RM && n_na_)
      return NA_REAL;

    double result;
    if (n_pos_inf_ && n_neg_inf_) result = R_NaN;
    else if (n_pos_inf_) result = R_PosInf;
    else if (n_neg_inf_) result = R_NegInf;
    else result = total_.value();

    if (!IS_MEAN)
      return result;

    // mean() divides by the number of values it actually saw; an empty window
    // gives the NaN that 0 / 0 produces, as it did before
    return result / (n_finite_ + n_pos_inf_ + n_neg_inf_);
  }

private:

  CompensatedSum total_;
  int n_finite_;
  int n_na_;
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
  VarAccumulator(Callable, NumericVector const& x, int) : Base(x) {
    clear();
  }

  // two running sums against two passes over the window, so this pays off
  // sooner than a plain total does; as above, the crossover scales with 'by'
  static bool worthwhile(int n, int by) { return n >= 12LL * by; }

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

  // Centre on the window's own mean. Deviations are then as small as they can
  // be, which is what keeps both the running sums and the subtraction above
  // well conditioned; add() would otherwise fall back on the first value it
  // saw, which can sit arbitrarily far from the mean.
  void prepare(int start, int end) {
    double total = 0.0;
    int count = 0;
    for (int i = start; i <= end; ++i) {
      double value = this->x_[i];
      if (!ISNAN(value) && R_FINITE(value)) {
        total += value;
        ++count;
      }
    }
    shift_ = count ? total / count : 0.0;
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
    if (ISNAN(value)) { ++n_na_; return; }
    if (!R_FINITE(value)) { ++n_infinite_; return; }
    if (!have_shift_) { shift_ = value; have_shift_ = true; }
    double difference = value - shift_;
    s1_.add(difference);
    s2_.add(difference * difference);
    ++n_finite_;
  }

  void remove(int i) {
    double value = this->x_[i];
    if (ISNAN(value)) { --n_na_; return; }
    if (!R_FINITE(value)) { --n_infinite_; return; }
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

// Monotonic deque of candidate indices behind min() and max(): the front is
// always the extremum of the current window, and an index is dropped as soon
// as a later observation beats it. Ties are broken the way the from-scratch
// loops broke them, so that the sign of a zero carries through unchanged.
template <bool NA_RM, bool IS_MIN>
class ExtremumAccumulator :
  public WindowAccumulator< ExtremumAccumulator<NA_RM, IS_MIN> > {

  typedef WindowAccumulator< ExtremumAccumulator<NA_RM, IS_MIN> > Base;

public:

  template <typename Callable>
  ExtremumAccumulator(Callable, NumericVector const& x, int) : Base(x) {
    clear();
  }

  // Maintaining the deque costs more than two comparisons would. Beyond the
  // by = 1 case the deque loses ground faster than the sums above do -- each
  // slid observation pays its bookkeeping, but the answer still needs the
  // front inspected and the departures matched -- so its crossover carries a
  // steeper measured slope.
  static bool worthwhile(int n, int by) {
    return by == 1 ? n >= 4 : n >= 8LL * by;
  }

  // comparisons are exact, so there is nothing to lose
  bool degraded() const { return false; }

  void prepare(int, int) {}

  void clear() {
    candidates_.clear();
    n_na_ = 0;
  }

  void add(int i) {
    double value = this->x_[i];
    if (ISNAN(value)) { ++n_na_; return; }
    while (!candidates_.empty() && beats(value, this->x_[candidates_.back()]))
      candidates_.pop_back();
    candidates_.push_back(i);
  }

  void remove(int i) {
    if (ISNAN(this->x_[i])) { --n_na_; return; }
    if (!candidates_.empty() && candidates_.front() == i)
      candidates_.pop_front();
  }

  double value() const {
    if (!NA_RM && n_na_)
      return NA_REAL;
    // an empty window keeps the identity the from-scratch loops started from
    if (candidates_.empty())
      return IS_MIN ? R_PosInf : R_NegInf;
    return this->x_[candidates_.front()];
  }

private:

  // min() kept the earlier of two equal values, max() the later
  static bool beats(double candidate, double incumbent) {
    return IS_MIN ? candidate < incumbent : candidate >= incumbent;
  }

  std::deque<int> candidates_;
  int n_na_;

};

// The window is kept in sorted order, so the median is a lookup and each step
// costs a binary search plus one memmove -- cheaper than selecting the middle
// element out of the whole window every time.
template <bool NA_RM>
class MedianAccumulator :
  public WindowAccumulator< MedianAccumulator<NA_RM> > {

  typedef WindowAccumulator< MedianAccumulator<NA_RM> > Base;

public:

  template <typename Callable>
  MedianAccumulator(Callable, NumericVector const& x, int n) : Base(x) {
    if (n > 0) sorted_.reserve(n);
    clear();
  }

  // Sorted insertion beats re-selecting the middle even for a pair, but each
  // step pays 'by' insertions and removals of ~n/2 elements each where the
  // selection pays one pass whatever 'by' is; the measured crossover sits at
  // 'by' about a quarter of 'n', independent of 'n'.
  static bool worthwhile(int n, int by) {
    return by == 1 || n > 4LL * by;
  }

  // the window is carried in full rather than summarized, so likewise
  bool degraded() const { return false; }

  void prepare(int, int) {}

  void clear() {
    sorted_.clear();
    n_na_ = 0;
  }

  void add(int i) {
    double value = this->x_[i];
    if (ISNAN(value)) { ++n_na_; return; }
    sorted_.insert(
      std::lower_bound(sorted_.begin(), sorted_.end(), value), value);
  }

  void remove(int i) {
    double value = this->x_[i];
    if (ISNAN(value)) { --n_na_; return; }
    std::vector<double>::iterator it =
      std::lower_bound(sorted_.begin(), sorted_.end(), value);
    if (it != sorted_.end() && *it == value)
      sorted_.erase(it);
  }

  double value() const {
    if (!NA_RM && n_na_)
      return NA_REAL;
    size_t k = sorted_.size();
    if (k == 0)
      return NA_REAL;
    if (k % 2 == 0)
      return (sorted_[k / 2 - 1] + sorted_[k / 2]) / 2;
    return sorted_[k / 2];
  }

private:

  std::vector<double> sorted_;
  int n_na_;

};

// ---------------------------------------------------------------------------
// Windowing functions
//
// These compute a window from scratch. They still carry the weighted forms,
// which have no incremental equivalent -- a weight belongs to a position
// within the window, so sliding the window re-pairs every weight with a
// different observation.
// ---------------------------------------------------------------------------

template <bool NA_RM>
struct mean_f;

template <>
struct mean_f<true> {
  inline double operator()(NumericVector const& x, int offset, int n) {
    double result = 0.0;
    int num = 0;
    for (int i = 0; i < n; ++i) {
      if (!ISNAN(x[offset + i])) {
        result += x[offset + i];
        ++num;
      }
    }
    return result / num;
  }

  inline double operator()(NumericVector const& x,
                           int offset,
                           NumericVector const& weights,
                           int n) {
    // NOTE: the weights need to be re-normalized after dropping NAs, so we
    // divide by the sum of the weights actually used rather than by a count
    double result = 0.0;
    double weights_sum = 0.0;
    for (int i = 0; i < n; ++i) {
      if (!ISNAN(x[offset + i])) {
        result += x[offset + i] * weights[i];
        weights_sum += weights[i];
      }
    }
    return result / weights_sum;
  }
};

template <>
struct mean_f<false> {
  inline double operator()(NumericVector const& x, int offset, int n) {
    double result = 0.0;
    for (int i = 0; i < n; ++i) {
      result += x[offset + i];
    }
    return result / n;
  }

  inline double operator()(NumericVector const& x,
                           int offset,
                           NumericVector const& weights,
                           int n) {
    double result = 0.0;
    for (int i = 0; i < n; ++i) {
      result += x[offset + i] * weights[i];
    }
    return result / n;
  }
};

template <bool NA_RM>
struct sum_f;

template <>
struct sum_f<false> {

  inline double operator()(NumericVector const& x, int offset, int n) {
    double result = 0.0;
    for (int i = 0; i < n; ++i) {
      result += x[offset + i];
    }
    return result;
  }

  inline double operator()(NumericVector const& x,
                           int offset,
                           NumericVector const& weights,
                           int n) {
    double result = 0.0;
    for (int i = 0; i < n; ++i) {
      result += x[offset + i] * weights[i];
    }
    return result;
  }

};

template <>
struct sum_f<true> {

  inline double operator()(NumericVector const& x, int offset, int n) {
    double result = 0.0;
    for (int i = 0; i < n; ++i) {
      if (!ISNAN(x[offset + i])) {
        result += x[offset + i];
      }
    }
    return result;
  }

  inline double operator()(NumericVector const& x,
                           int offset,
                           NumericVector const& weights,
                           int n) {
    double result = 0.0;
    for (int i = 0; i < n; ++i) {
      if (!ISNAN(x[offset + i])) {
        result += x[offset + i] * weights[i];
      }
    }
    return result;
  }

};

template <bool NA_RM>
struct min_f;

template <>
struct min_f<false> {

  inline double operator()(NumericVector const& x,
                           int offset,
                           int n) {
    double result = R_PosInf;
    for (int i = 0; i < n; ++i) {
      if (ISNAN(x[offset + i])) {
        return NA_REAL;
      }
      result = x[offset + i] < result ? x[offset + i] : result;
    }
    return result;
  }

  inline double operator()(NumericVector const& x,
                           int offset,
                           NumericVector const& weights,
                           int n) {
    double result = R_PosInf;
    for (int i = 0; i < n; ++i) {
      if (ISNAN(x[offset + i])) {
        return NA_REAL;
      }
#define VALUE (x[offset + i] * weights[i])
      result = VALUE < result ? VALUE : result;
#undef VALUE
    }
    return result;
  }

};

template <>
struct min_f<true> {

  inline double operator()(NumericVector const& x,
                           int offset,
                           NumericVector const& weights,
                           int n) {
    double result = R_PosInf;
    for (int i = 0; i < n; ++i) {
#define VALUE (x[offset + i] * weights[i])
      result = VALUE < result ? VALUE : result;
#undef VALUE
    }
    return result;
  }

  inline double operator()(NumericVector const& x,
                           int offset,
                           int n) {
    double result = R_PosInf;
    for (int i = 0; i < n; ++i) {
      result = x[offset + i] < result ? x[offset + i] : result;
    }
    return result;
  }
};

template <bool NA_RM>
struct max_f;

template <>
struct max_f<false> {

  inline double operator()(NumericVector const& x,
                           int offset,
                           NumericVector const& weights,
                           int n) {
    double result = R_NegInf;
    for (int i = 0; i < n; ++i) {
      if (ISNAN(x[offset + i])) {
        return NA_REAL;
      }
#define VALUE (x[offset + i] * weights[i])
      result = VALUE < result ? result : VALUE;
#undef VALUE
    }
    return result;
  }

  inline double operator()(NumericVector const& x,
                           int offset,
                           int n) {
    double result = R_NegInf;
    for (int i = 0; i < n; ++i) {
      if (ISNAN(x[offset + i])) {
        return NA_REAL;
      }
      result = x[offset + i] < result ? result : x[offset + i];
    }
    return result;
  }
};

template <>
struct max_f<true> {

  inline double operator()(NumericVector const& x,
                           int offset,
                           NumericVector const& weights,
                           int n) {
    double result = R_NegInf;
    for (int i = 0; i < n; ++i) {
      if (ISNAN(x[offset + i])) continue;
#define VALUE (x[offset + i] * weights[i])
      result = VALUE < result ? result : VALUE;
#undef VALUE
    }
    return result;
  }

  inline double operator()(NumericVector const& x,
                           int offset,
                           int n) {
    double result = R_NegInf;
    for (int i = 0; i < n; ++i) {
      if (ISNAN(x[offset + i])) continue;
      result = x[offset + i] < result ? result : x[offset + i];
    }
    return result;
  }
};

template <bool NA_RM>
struct prod_f;

template <>
struct prod_f<true> {

  inline double operator()(NumericVector const& x, int offset, int n) {
    double result = 1.0;
    for (int i = 0; i < n; ++i) {
      if (!ISNAN(x[offset + i])) {
        result *= x[offset + i];
      }
    }
    return result;
  }

  inline double operator()(NumericVector const& x,
                           int offset,
                           NumericVector const& weights,
                           int n) {
    double result = 1.0;
    for (int i = 0; i < n; ++i) {
      if (!ISNAN(x[offset + i])) {
        result *= x[offset + i] * weights[i];
      }
    }
    return result;
  }
};

template <>
struct prod_f<false> {

  inline double operator()(NumericVector const& x, int offset, int n) {
    double result = 1.0;
    for (int i = 0; i < n; ++i) {
      result *= x[offset + i];
    }
    return result;
  }

  inline double operator()(NumericVector const& x,
                           int offset,
                           NumericVector const& weights,
                           int n) {
    double result = 1.0;
    for (int i = 0; i < n; ++i) {
      result *= x[offset + i] * weights[i];
    }
    return result;
  }
};

// Compute a weighted median, ignoring any NAs in the window. The weights are
// tied to their associated values before sorting, since a weight applies to the
// value at its own position rather than to the value that ends up sorted there.
// 'scratch' belongs to the caller, so that a pass over many windows reuses one
// buffer rather than allocating for each of them.
inline double weighted_median(NumericVector const& x,
                              int offset,
                              NumericVector const& weights,
                              int n,
                              std::vector< std::pair<double, double> >& scratch) {

  scratch.clear();
  for (int i = 0; i < n; ++i) {
    double value = x[offset + i];
    if (!ISNAN(value))
      scratch.push_back(std::make_pair(value, weights[i]));
  }

  if (scratch.empty())
    return NA_REAL;

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

// Select the median out of 'scratch', which this reorders. std::nth_element
// places the middle value in linear time, where a partial sort of the lower
// half of the window would cost an extra log factor.
inline double select_median(std::vector<double>& scratch) {

  size_t n = scratch.size();
  if (n == 0)
    return NA_REAL;

  std::nth_element(
    scratch.begin(), scratch.begin() + n / 2, scratch.end());
  double upper = scratch[n / 2];

  if (n % 2 == 0) {
    // everything below the midpoint is already partitioned below it, so the
    // other middle value is simply the largest of that part
    double lower = *std::max_element(scratch.begin(), scratch.begin() + n / 2);
    return (lower + upper) / 2;
  }

  return upper;

}

template <bool NA_RM>
struct median_f;

template <>
struct median_f<false> {

  inline double operator()(NumericVector const& x, int offset, int n) {

    for (int i = offset; i < offset + n; i++)
      if (ISNAN(x[i]))
        return NA_REAL;

    scratch_.assign(x.begin() + offset, x.begin() + offset + n);
    return select_median(scratch_);

  }

  inline double operator()(NumericVector const& x,
                           int offset,
                           NumericVector const& weights,
                           int n) {

    for (int i = offset; i < offset + n; i++)
      if (ISNAN(x[i]))
        return NA_REAL;

    return weighted_median(x, offset, weights, n, weighted_scratch_);
  }

private:

  std::vector<double> scratch_;
  std::vector< std::pair<double, double> > weighted_scratch_;

};

template <>
struct median_f<true> {

  inline double operator()(NumericVector const& x, int offset, int n) {

    scratch_.clear();
    scratch_.reserve(n);
    for (int i = offset; i < offset + n; i++)
      if (!ISNAN(x[i]))
        scratch_.push_back(x[i]);

    return select_median(scratch_);

  }

  inline double operator()(NumericVector const& x,
                           int offset,
                           NumericVector const& weights,
                           int n) {

    return weighted_median(x, offset, weights, n, weighted_scratch_);
  }

private:

  std::vector<double> scratch_;
  std::vector< std::pair<double, double> > weighted_scratch_;

};

// Sample variance of a window, ignoring NAs. NA when fewer than two values
// remain, matching var()'s behaviour for a vector of length 0 or 1. The first
// pass also reports whether it saw an NA at all, so that the NA-intolerant
// form below does not need a scan of its own.
inline double window_var(NumericVector const& x,
                         int offset,
                         int n,
                         bool& has_na) {

  double total = 0.0;
  int count = 0;
  has_na = false;

  for (int i = 0; i < n; ++i) {
    double value = x[offset + i];
    if (ISNAN(value)) {
      has_na = true;
    } else {
      total += value;
      ++count;
    }
  }

  if (count < 2)
    return NA_REAL;

  double mean = total / count;

  // Corrected two-pass: the deviations are measured from a mean that is itself
  // rounded, and their total collects exactly the error that introduces --
  // subtracting it below takes the rounding back out. Without it a window
  // whose spread is small beside its mean loses most of its digits here.
  double squares = 0.0;
  double residual = 0.0;
  for (int i = 0; i < n; ++i) {
    double value = x[offset + i];
    if (!ISNAN(value)) {
      double difference = value - mean;
      squares += difference * difference;
      residual += difference;
    }
  }

  // the deviations squared past what a double can hold, so the variance is out
  // of range too -- and the correction below would only turn it into a NaN
  if (squares == R_PosInf)
    return R_PosInf;

  double result = squares - residual * residual / count;
  if (result < 0.0) result = 0.0;

  return result / (count - 1);

}

// Weighted sample variance, treating a weight as a repeat count (frequency
// weights):
//
//   m  = sum(w * x) / sum(w)
//   s2 = sum(w * (x - m)^2) / (sum(w) - 1)
//
// Since 'normalize' scales the weights to sum to n, equal weights reduce this
// to window_var() above, so a uniform weight vector agrees with the unweighted
// routines. NAs are dropped from the values and their own weights together.
inline double weighted_var(NumericVector const& x,
                           int offset,
                           NumericVector const& weights,
                           int n,
                           bool& has_na) {

  double weights_sum = 0.0;
  double weighted_total = 0.0;
  int count = 0;
  has_na = false;

  for (int i = 0; i < n; ++i) {
    double value = x[offset + i];
    if (ISNAN(value)) {
      has_na = true;
    } else {
      weights_sum += weights[i];
      weighted_total += weights[i] * value;
      ++count;
    }
  }

  // as above for fewer than two values; a denominator that is zero or negative
  // (possible with 'normalize = FALSE', or after dropping NAs) has no
  // meaningful answer either
  if (count < 2 || !(weights_sum > 1))
    return NA_REAL;

  double mean = weighted_total / weights_sum;

  // corrected two-pass, as above, with the weights carried through
  double squares = 0.0;
  double residual = 0.0;
  for (int i = 0; i < n; ++i) {
    double value = x[offset + i];
    if (!ISNAN(value)) {
      double difference = value - mean;
      squares += weights[i] * difference * difference;
      residual += weights[i] * difference;
    }
  }

  // the deviations squared past what a double can hold, so the variance is out
  // of range too -- and the correction below would only turn it into a NaN
  if (squares == R_PosInf)
    return R_PosInf;

  double result = squares - residual * residual / weights_sum;
  if (result < 0.0) result = 0.0;

  return result / (weights_sum - 1);

}

template <bool NA_RM>
struct var_f;

template <>
struct var_f<false> {

  inline double operator()(NumericVector const& x, int offset, int n) {
    bool has_na;
    double result = window_var(x, offset, n, has_na);
    return has_na ? NA_REAL : result;
  }

  inline double operator()(NumericVector const& x,
                           int offset,
                           NumericVector const& weights,
                           int n) {
    bool has_na;
    double result = weighted_var(x, offset, weights, n, has_na);
    return has_na ? NA_REAL : result;
  }

};

template <>
struct var_f<true> {

  inline double operator()(NumericVector const& x, int offset, int n) {
    bool has_na;
    return window_var(x, offset, n, has_na);
  }

  inline double operator()(NumericVector const& x,
                           int offset,
                           NumericVector const& weights,
                           int n) {
    bool has_na;
    return weighted_var(x, offset, weights, n, has_na);
  }

};

template <bool NA_RM>
struct sd_f;

template <>
struct sd_f<false> {

  inline double operator()(NumericVector const& x, int offset, int n) {
    return window_sqrt(var_f<false>()(x, offset, n));
  }

  inline double operator()(NumericVector const& x,
                           int offset,
                           NumericVector const& weights,
                           int n) {
    return window_sqrt(var_f<false>()(x, offset, weights, n));
  }

};

template <>
struct sd_f<true> {

  inline double operator()(NumericVector const& x, int offset, int n) {
    return window_sqrt(var_f<true>()(x, offset, n));
  }

  inline double operator()(NumericVector const& x,
                           int offset,
                           NumericVector const& weights,
                           int n) {
    return window_sqrt(var_f<true>()(x, offset, weights, n));
  }

};

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
struct accumulator_for< var_f<NA_RM> > {
  typedef VarAccumulator<NA_RM, false> type;
};

template <bool NA_RM>
struct accumulator_for< sd_f<NA_RM> > {
  typedef VarAccumulator<NA_RM, true> type;
};

template <bool NA_RM>
struct accumulator_for< median_f<NA_RM> > {
  typedef MedianAccumulator<NA_RM> type;
};

// ---------------------------------------------------------------------------
// Drivers
//
// Each writes 'rollOutputSize()' values into a buffer the caller owns, so that
// the matrix routine can hand over a column of its output directly.
// ---------------------------------------------------------------------------

// Walk the clipped windows, writing one value per point.
template <typename Accumulator, typename Callable>
void roll_partial_windows(Callable f,
                          NumericVector const& x,
                          double* output,
                          int n,
                          int by,
                          int leftOffset,
                          int rightOffset) {

  int x_n = x.size();
  Accumulator accumulator(f, x, n);

  for (int i = 0; i < x_n; i += by) {
    int start = i - leftOffset;
    int end   = i + rightOffset;
    if (start < 0) start = 0;
    if (end > x_n - 1) end = x_n - 1;
    output[i] = accumulator.compute(start, end);
  }

}

template <typename Callable>
void roll_partial_direct(Callable f,
                         NumericVector const& x,
                         double* output,
                         int n,
                         int by,
                         int leftOffset,
                         int rightOffset) {

  int x_n = x.size();

  for (int i = 0; i < x_n; i += by) {
    int start = i - leftOffset;
    int end   = i + rightOffset;
    if (start < 0) start = 0;
    if (end > x_n - 1) end = x_n - 1;
    output[i] = f(x, start, end - start + 1);
  }

}

// Windows clipped to the bounds of 'x': every point gets an answer, computed
// over however many observations are in range. The point a window is reported
// at is always in range itself, so a window is never empty.
template <typename Callable>
void roll_vector_partial_into(Callable f,
                              NumericVector const& x,
                              double* output,
                              int n,
                              int by,
                              String const& align) {

  int leftOffset  = getLeftOffset(align, n);
  int rightOffset = getRightOffset(align, n);

  // points we skip over are not computed, and 'fill' does not apply here
  if (by != 1)
    std::fill(output, output + x.size(), NA_REAL);

  typedef typename accumulator_for<Callable>::type Incremental;
  if (Incremental::worthwhile(n, by))
    roll_partial_windows<Incremental>(
      f, x, output, n, by, leftOffset, rightOffset);
  else
    roll_partial_direct(f, x, output, n, by, leftOffset, rightOffset);

}

// Walk the whole windows, writing 'output' from 'from' up to 'to'. Returns one
// step past the last window computed, which is where the right-hand fill picks
// up.
template <typename Accumulator, typename Callable>
int roll_fill_windows(Callable f,
                      NumericVector const& x,
                      double* output,
                      int n,
                      int by,
                      int from,
                      int to,
                      int padLeftTimes) {

  Accumulator accumulator(f, x, n);

  int i = from;
  for (; i < to; i += by) {
    int start = i - padLeftTimes;
    output[i] = accumulator.compute(start, start + n - 1);
  }

  return i;
}

template <typename Callable>
int roll_fill_direct(Callable f,
                     NumericVector const& x,
                     double* output,
                     int n,
                     int by,
                     int from,
                     int to,
                     int padLeftTimes) {

  int i = from;
  for (; i < to; i += by)
    output[i] = f(x, i - padLeftTimes, n);

  return i;
}

template <typename Callable>
void roll_vector_fill_into(Callable f,
                           NumericVector const& x,
                           double* output,
                           int n,
                           NumericVector const& weights,
                           int by,
                           Fill const& fill,
                           String const& align) {

  int x_n = x.size();

  if (x_n < n) {
    std::fill(output, output + x_n, NA_REAL);
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

  // Fill result -- we hoist the indexing variable outside of the loop
  // so we can re-use it to easily figure out where our 'fill-right'
  // pass-through should start
  if (weights.size()) {
    for (; i < padLeftTimes + ops_n; i += by) {
      output[i] = f(x, i - padLeftTimes, weights, n);
    }
  } else {
    typedef typename accumulator_for<Callable>::type Incremental;
    i = Incremental::worthwhile(n, by) ?
      roll_fill_windows<Incremental>(
        f, x, output, n, by, i, padLeftTimes + ops_n, padLeftTimes) :
      roll_fill_direct(
        f, x, output, n, by, i, padLeftTimes + ops_n, padLeftTimes);
  }

  // Fill-right on the remainders. We move the index
  // back one 'by' iteration, then move it back one.
  i -= by;
  ++i;
  for (; i < output_n; ++i)
    output[i] = fill.right();

}

template <typename Accumulator, typename Callable>
void roll_nofill_windows(Callable f,
                         NumericVector const& x,
                         double* output,
                         int n,
                         int by,
                         int output_n) {

  Accumulator accumulator(f, x, n);

  int index = 0;
  for (int i = 0; i < output_n; ++i) {
    output[i] = accumulator.compute(index, index + n - 1);
    index += by;
  }
}

template <typename Callable>
void roll_nofill_direct(Callable f,
                        NumericVector const& x,
                        double* output,
                        int n,
                        int by,
                        int output_n) {

  int index = 0;
  for (int i = 0; i < output_n; ++i) {
    output[i] = f(x, index, n);
    index += by;
  }
}

template <typename Callable>
void roll_vector_nofill_into(Callable f,
                             NumericVector const& x,
                             double* output,
                             int n,
                             NumericVector const& weights,
                             int by) {

  int output_n = (x.size() - n) / by + 1;

  int index = 0;
  if (weights.size()) {
    for (int i = 0; i < output_n; ++i) {
      output[i] = f(x, index, weights, n);
      index += by;
    }
  } else {
    typedef typename accumulator_for<Callable>::type Incremental;
    if (Incremental::worthwhile(n, by))
      roll_nofill_windows<Incremental>(f, x, output, n, by, output_n);
    else
      roll_nofill_direct(f, x, output, n, by, output_n);
  }

}

template <typename Callable>
void roll_vector_into(Callable f,
                      NumericVector const& x,
                      double* output,
                      int n,
                      NumericVector const& weights,
                      int by,
                      Fill const& fill,
                      bool partial,
                      String const& align) {

  // partial windows are computable at every point, so there is nothing to
  // shorten or to pad; 'weights' is rejected upstream in this case
  if (partial)
    roll_vector_partial_into(f, x, output, n, by, align);
  else if (fill.filled())
    roll_vector_fill_into(f, x, output, n, weights, by, fill, align);
  else
    roll_vector_nofill_into(f, x, output, n, weights, by);

}

template <typename Callable>
NumericVector roll_vector_with(Callable f,
                               NumericVector const& x,
                               int n,
                               NumericVector const& weights,
                               int by,
                               Fill const& fill,
                               bool partial,
                               String const& align) {

  NumericVector output =
    no_init(rollOutputSize(x.size(), n, by, fill, partial));

  roll_vector_into(
    f, x, output.begin(), n, weights, by, fill, partial, align);

  return output;
}

// A NumericMatrix keeps its data column-major and contiguous, so a column can
// be copied into one reused buffer and its results written straight into the
// output -- rather than allocating a vector in and a vector out per column.
template <typename Callable>
NumericMatrix roll_matrix_with(Callable f,
                               NumericMatrix x,
                               int n,
                               NumericVector const& weights,
                               int by,
                               Fill const& fill,
                               bool partial,
                               String const& align) {

  int nrow = x.nrow();
  int ncol = x.ncol();
  int output_nrow = rollOutputSize(nrow, n, by, fill, partial);

  NumericMatrix output(output_nrow, ncol);
  NumericVector column = no_init(nrow);

  double const* source = x.begin();
  double* target = output.begin();

  for (int j = 0; j < ncol; ++j) {
    std::copy(source + (R_xlen_t) j * nrow,
              source + (R_xlen_t) (j + 1) * nrow,
              column.begin());
    roll_vector_into(f, column, target + (R_xlen_t) j * output_nrow,
                     n, weights, by, fill, partial, align);
  }

  return output;
}

// Shared entry point for the generated exports below: 'weights' settles the
// window size, and is normalized once here rather than once per column.
template <typename Callable>
SEXP roll_with(Callable f,
               SEXP data,
               int n,
               NumericVector const& weights,
               int by,
               Fill const& fill,
               bool partial,
               String const& align,
               bool normalize) {

  // Normalize 'n' to match that of weights
  if (weights.size())
    n = weights.size();

  NumericVector scaled = normalizeWeights(weights, n, normalize);

  if (Rf_isMatrix(data))
    return roll_matrix_with(
      f, NumericMatrix(data), n, scaled, by, fill, partial, align);

  return roll_vector_with(
    f, NumericVector(data), n, scaled, by, fill, partial, align);

}

}  // end namespace RcppRoll

// [[Rcpp::export]]
NumericVector na_locf(NumericVector x)
{
  NumericVector output = Rcpp::clone(x);

  double lastNonNA = NA_REAL;
  int n = x.size();

  for (int i = 0; i < n; ++i)
  {
    double value = output[i];
    if (!ISNAN(value))
      lastNonNA = value;
    else
      output[i] = lastNonNA;
  }
  return output;
}

// Begin auto-generated exports (internal/make-exports.R)

// [[Rcpp::export]]
SEXP roll_mean_impl(SEXP x,
             int n,
             NumericVector weights,
             int by,
             NumericVector fill_,
             bool partial,
             String align,
             bool normalize,
             bool na_rm)
{
  RcppRoll::Fill fill(fill_);
  if (na_rm) {
    return RcppRoll::roll_with(
      RcppRoll::mean_f<true>(), x, n, weights, by, fill, partial, align, normalize);
  } else {
    return RcppRoll::roll_with(
      RcppRoll::mean_f<false>(), x, n, weights, by, fill, partial, align, normalize);
  }
}
// [[Rcpp::export]]
SEXP roll_median_impl(SEXP x,
             int n,
             NumericVector weights,
             int by,
             NumericVector fill_,
             bool partial,
             String align,
             bool normalize,
             bool na_rm)
{
  RcppRoll::Fill fill(fill_);
  if (na_rm) {
    return RcppRoll::roll_with(
      RcppRoll::median_f<true>(), x, n, weights, by, fill, partial, align, normalize);
  } else {
    return RcppRoll::roll_with(
      RcppRoll::median_f<false>(), x, n, weights, by, fill, partial, align, normalize);
  }
}
// [[Rcpp::export]]
SEXP roll_min_impl(SEXP x,
             int n,
             NumericVector weights,
             int by,
             NumericVector fill_,
             bool partial,
             String align,
             bool normalize,
             bool na_rm)
{
  RcppRoll::Fill fill(fill_);
  if (na_rm) {
    return RcppRoll::roll_with(
      RcppRoll::min_f<true>(), x, n, weights, by, fill, partial, align, normalize);
  } else {
    return RcppRoll::roll_with(
      RcppRoll::min_f<false>(), x, n, weights, by, fill, partial, align, normalize);
  }
}
// [[Rcpp::export]]
SEXP roll_max_impl(SEXP x,
             int n,
             NumericVector weights,
             int by,
             NumericVector fill_,
             bool partial,
             String align,
             bool normalize,
             bool na_rm)
{
  RcppRoll::Fill fill(fill_);
  if (na_rm) {
    return RcppRoll::roll_with(
      RcppRoll::max_f<true>(), x, n, weights, by, fill, partial, align, normalize);
  } else {
    return RcppRoll::roll_with(
      RcppRoll::max_f<false>(), x, n, weights, by, fill, partial, align, normalize);
  }
}
// [[Rcpp::export]]
SEXP roll_prod_impl(SEXP x,
             int n,
             NumericVector weights,
             int by,
             NumericVector fill_,
             bool partial,
             String align,
             bool normalize,
             bool na_rm)
{
  RcppRoll::Fill fill(fill_);
  if (na_rm) {
    return RcppRoll::roll_with(
      RcppRoll::prod_f<true>(), x, n, weights, by, fill, partial, align, normalize);
  } else {
    return RcppRoll::roll_with(
      RcppRoll::prod_f<false>(), x, n, weights, by, fill, partial, align, normalize);
  }
}
// [[Rcpp::export]]
SEXP roll_sum_impl(SEXP x,
             int n,
             NumericVector weights,
             int by,
             NumericVector fill_,
             bool partial,
             String align,
             bool normalize,
             bool na_rm)
{
  RcppRoll::Fill fill(fill_);
  if (na_rm) {
    return RcppRoll::roll_with(
      RcppRoll::sum_f<true>(), x, n, weights, by, fill, partial, align, normalize);
  } else {
    return RcppRoll::roll_with(
      RcppRoll::sum_f<false>(), x, n, weights, by, fill, partial, align, normalize);
  }
}
// [[Rcpp::export]]
SEXP roll_sd_impl(SEXP x,
             int n,
             NumericVector weights,
             int by,
             NumericVector fill_,
             bool partial,
             String align,
             bool normalize,
             bool na_rm)
{
  RcppRoll::Fill fill(fill_);
  if (na_rm) {
    return RcppRoll::roll_with(
      RcppRoll::sd_f<true>(), x, n, weights, by, fill, partial, align, normalize);
  } else {
    return RcppRoll::roll_with(
      RcppRoll::sd_f<false>(), x, n, weights, by, fill, partial, align, normalize);
  }
}
// [[Rcpp::export]]
SEXP roll_var_impl(SEXP x,
             int n,
             NumericVector weights,
             int by,
             NumericVector fill_,
             bool partial,
             String align,
             bool normalize,
             bool na_rm)
{
  RcppRoll::Fill fill(fill_);
  if (na_rm) {
    return RcppRoll::roll_with(
      RcppRoll::var_f<true>(), x, n, weights, by, fill, partial, align, normalize);
  } else {
    return RcppRoll::roll_with(
      RcppRoll::var_f<false>(), x, n, weights, by, fill, partial, align, normalize);
  }
}
// End auto-generated exports (internal/make-exports.R)
