#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include <algorithm>
#include <cmath>
#include <cstring>
#include <deque>
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
  switch (Rf_length(vector)) {
    case 0: {
      filled_ = false;
      break;
    }
    case 1: {
      left_ = middle_ = right_ = REAL(vector)[0];
      filled_ = true;
      break;
    }
    case 3: {
      double const* data = REAL(vector);
      left_ = data[0];
      middle_ = data[1];
      right_ = data[2];
      filled_ = true;
      break;
    }
    default: {
      Rf_error("'fill' should be a vector of size 0, 1, or 3");
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

  std::vector<double> scaled(weights, weights + weights_n);
  if (!normalize || !weights_n)
    return scaled;

  double total = 0.0;
  for (int i = 0; i < weights_n; ++i)
    total += weights[i];

  for (int i = 0; i < weights_n; ++i)
    scaled[i] = weights[i] / total * n;

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

  WindowAccumulator(double const* x)
    : x_(x), start_(0), end_(-1), credit_(0) {}

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

protected:

  double const* x_;
  int start_;
  int end_;
  int credit_;

};

// Recomputes the window on every call: the fallback whenever carrying state
// forward is not worthwhile, and the only form an operation without an
// incremental equivalent ever takes. Adapting the callable to the same
// compute(start, end) protocol lets one set of window-walking drivers serve
// both paths.
template <typename Callable>
class DirectAccumulator {

public:

  DirectAccumulator(Callable f, double const* x, int /* n */)
    : f_(f), x_(x) {}

  // an operation with no incremental form always reads the window afresh
  static bool worthwhile(int, int) { return false; }

  double compute(int start, int end) {
    return f_(x_, start, end - start + 1);
  }

private:

  Callable f_;
  double const* x_;

};

// Adapts the weighted form of a windowing function to the same
// compute(start, end) protocol, so the chunked drivers below serve the
// weighted path too. Weighted windows are never clipped -- 'weights' is
// rejected together with 'partial' -- so the width recovered here is always
// the weights' own length.
template <typename Callable>
class WeightedAccumulator {

public:

  WeightedAccumulator(Callable f, double const* x, double const* weights)
    : f_(f), x_(x), weights_(weights) {}

  double compute(int start, int end) {
    return f_(x_, start, weights_, end - start + 1);
  }

private:

  Callable f_;
  double const* x_;
  double const* weights_;

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
  SumAccumulator(Callable, double const* x, int) : Base(x) {
    clear();
  }

  // One add and one subtract per observation entering or leaving, against a
  // from-scratch pass the compiler vectorizes well now that it runs over a
  // plain pointer. Each step slides the window 'by' observations, so the
  // crossover scales with 'by' -- and a 'by' past the crossover includes
  // every 'by' wide enough to leave gaps, where there is nothing to carry
  // forward at all.
  static bool worthwhile(int n, int by) { return n >= 56LL * by; }

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
  VarAccumulator(Callable, double const* x, int) : Base(x) {
    clear();
  }

  // two running sums against two passes over the window, so this pays off
  // sooner than a plain total does; as above, the crossover scales with 'by'
  static bool worthwhile(int n, int by) { return n >= 16LL * by; }

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
    double total = 0.0;
    int count = 0;
    for (int i = start; i <= end; ++i) {
      double value = this->x_[i];
      if (is_finite(value)) {
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
  ExtremumAccumulator(Callable, double const* x, int) : Base(x) {
    clear();
  }

  // maintaining the deque costs more than a stretch of comparisons the
  // compiler can turn into branchless minimums; each step slides the window
  // 'by' observations, so the crossover scales with 'by' as above
  static bool worthwhile(int n, int by) { return n >= 32LL * by; }

  // comparisons are exact, so there is nothing to lose
  bool degraded() const { return false; }
  bool urgent() const { return false; }

  void prepare(int, int) {}

  void clear() {
    candidates_.clear();
    n_na_ = 0;
  }

  void add(int i) {
    double value = this->x_[i];
    if (is_nan(value)) { ++n_na_; return; }
    while (!candidates_.empty() && beats(value, this->x_[candidates_.back()]))
      candidates_.pop_back();
    candidates_.push_back(i);
  }

  void remove(int i) {
    if (is_nan(this->x_[i])) { --n_na_; return; }
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
    : Base(x), lower_order_(x), upper_order_(x) {

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
  static bool worthwhile(int n, int by) {
    return by == 1 || n > 4LL * by;
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
      return (sorted_[k / 2 - 1] + sorted_[k / 2]) / 2;
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
    return (lower_top + this->x_[upper_.front()]) / 2;
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

// Running product behind prod(). A slid window would have to divide out each
// departing value, and division cannot be trusted with the job: a zero has no
// inverse, an overflow or underflow is absorbing, and even where it is
// defined, dividing reintroduces rounding the original multiplication never
// had. The window is carried as two stacks instead -- values multiply into a
// running back product as they arrive, and when the oldest value must leave,
// the back stack is flipped once into suffix products, from which each
// removal is a pop. Every observation is touched at most twice, so a slide
// still costs O(1) amortized, and no product outlives the observations that
// made it: a zero or an infinity is gone from the state the moment the flip
// walks past it.
template <bool NA_RM>
class ProdAccumulator :
  public WindowAccumulator< ProdAccumulator<NA_RM> > {

  typedef WindowAccumulator< ProdAccumulator<NA_RM> > Base;

public:

  template <typename Callable>
  ProdAccumulator(Callable, double const* x, int n) : Base(x) {
    if (n > 0) {
      back_.reserve(n);
      suffix_.reserve(n);
    }
    clear();
  }

  // one multiply entering and an amortized one leaving, against a
  // from-scratch pass the compiler vectorizes well; as elsewhere, each step
  // slides the window 'by' observations, so the crossover scales with 'by'
  static bool worthwhile(int n, int by) { return n >= 24LL * by; }

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
  }

  void add(int i) {
    double value = this->x_[i];
    if (is_nan(value)) { if (ISNA(value)) ++n_na_; else ++n_nan_; return; }
    back_.push_back(value);
    back_product_ *= value;
  }

  void remove(int i) {
    double value = this->x_[i];
    if (is_nan(value)) { if (ISNA(value)) --n_na_; else --n_nan_; return; }

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

    double front = suffix_.empty() ? 1.0 : suffix_.back();
    return front * back_product_;
  }

private:

  std::vector<double> back_;    // values since the last flip, oldest first
  std::vector<double> suffix_;  // suffix products, the oldest value's on top
  double back_product_;
  int n_na_;
  int n_nan_;

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
  inline double operator()(double const* x, int offset, int n) {
    double result = 0.0;
    int num = 0;
    for (int i = 0; i < n; ++i) {
      if (!is_nan(x[offset + i])) {
        result += x[offset + i];
        ++num;
      }
    }
    return result / num;
  }

  inline double operator()(double const* x,
                           int offset,
                           double const* weights,
                           int n) {
    // NOTE: the weights need to be re-normalized after dropping NAs, so we
    // divide by the sum of the weights actually used rather than by a count
    double result = 0.0;
    double weights_sum = 0.0;
    for (int i = 0; i < n; ++i) {
      if (!is_nan(x[offset + i])) {
        result += x[offset + i] * weights[i];
        weights_sum += weights[i];
      }
    }
    return result / weights_sum;
  }
};

template <>
struct mean_f<false> {
  inline double operator()(double const* x, int offset, int n) {
    double result = 0.0;
    for (int i = 0; i < n; ++i) {
      result += x[offset + i];
    }
    return result / n;
  }

  inline double operator()(double const* x,
                           int offset,
                           double const* weights,
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

  inline double operator()(double const* x, int offset, int n) {
    double result = 0.0;
    for (int i = 0; i < n; ++i) {
      result += x[offset + i];
    }
    return result;
  }

  inline double operator()(double const* x,
                           int offset,
                           double const* weights,
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

  inline double operator()(double const* x, int offset, int n) {
    double result = 0.0;
    for (int i = 0; i < n; ++i) {
      if (!is_nan(x[offset + i])) {
        result += x[offset + i];
      }
    }
    return result;
  }

  inline double operator()(double const* x,
                           int offset,
                           double const* weights,
                           int n) {
    double result = 0.0;
    for (int i = 0; i < n; ++i) {
      if (!is_nan(x[offset + i])) {
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

  inline double operator()(double const* x,
                           int offset,
                           int n) {
    double result = R_PosInf;
    for (int i = 0; i < n; ++i) {
      if (is_nan(x[offset + i])) {
        return NA_REAL;
      }
      result = x[offset + i] < result ? x[offset + i] : result;
    }
    return result;
  }

  inline double operator()(double const* x,
                           int offset,
                           double const* weights,
                           int n) {
    double result = R_PosInf;
    for (int i = 0; i < n; ++i) {
      if (is_nan(x[offset + i])) {
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

  inline double operator()(double const* x,
                           int offset,
                           double const* weights,
                           int n) {
    double result = R_PosInf;
    for (int i = 0; i < n; ++i) {
#define VALUE (x[offset + i] * weights[i])
      result = VALUE < result ? VALUE : result;
#undef VALUE
    }
    return result;
  }

  inline double operator()(double const* x,
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

  inline double operator()(double const* x,
                           int offset,
                           double const* weights,
                           int n) {
    double result = R_NegInf;
    for (int i = 0; i < n; ++i) {
      if (is_nan(x[offset + i])) {
        return NA_REAL;
      }
#define VALUE (x[offset + i] * weights[i])
      result = VALUE < result ? result : VALUE;
#undef VALUE
    }
    return result;
  }

  inline double operator()(double const* x,
                           int offset,
                           int n) {
    double result = R_NegInf;
    for (int i = 0; i < n; ++i) {
      if (is_nan(x[offset + i])) {
        return NA_REAL;
      }
      result = x[offset + i] < result ? result : x[offset + i];
    }
    return result;
  }
};

template <>
struct max_f<true> {

  inline double operator()(double const* x,
                           int offset,
                           double const* weights,
                           int n) {
    double result = R_NegInf;
    for (int i = 0; i < n; ++i) {
      if (is_nan(x[offset + i])) continue;
#define VALUE (x[offset + i] * weights[i])
      result = VALUE < result ? result : VALUE;
#undef VALUE
    }
    return result;
  }

  inline double operator()(double const* x,
                           int offset,
                           int n) {
    double result = R_NegInf;
    for (int i = 0; i < n; ++i) {
      if (is_nan(x[offset + i])) continue;
      result = x[offset + i] < result ? result : x[offset + i];
    }
    return result;
  }
};

template <bool NA_RM>
struct prod_f;

template <>
struct prod_f<true> {

  inline double operator()(double const* x, int offset, int n) {
    double result = 1.0;
    for (int i = 0; i < n; ++i) {
      if (!is_nan(x[offset + i])) {
        result *= x[offset + i];
      }
    }
    return result;
  }

  inline double operator()(double const* x,
                           int offset,
                           double const* weights,
                           int n) {
    double result = 1.0;
    for (int i = 0; i < n; ++i) {
      if (!is_nan(x[offset + i])) {
        result *= x[offset + i] * weights[i];
      }
    }
    return result;
  }
};

template <>
struct prod_f<false> {

  inline double operator()(double const* x, int offset, int n) {
    double result = 1.0;
    for (int i = 0; i < n; ++i) {
      result *= x[offset + i];
    }
    return result;
  }

  inline double operator()(double const* x,
                           int offset,
                           double const* weights,
                           int n) {
    double result = 1.0;
    for (int i = 0; i < n; ++i) {
      result *= x[offset + i] * weights[i];
    }
    return result;
  }
};

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

    // split off the values below and above the pivot at the two ends of the
    // other buffer, keeping the pivot's run whole so that repeated values
    // cannot stall the descent
    size_t n_lt = 0;
    size_t n_gt = 0;
    double weight_lt = 0.0;
    double weight_eq = 0.0;

    for (size_t i = 0; i < size; ++i) {
      double value = from[i].first;
      if (value < pivot) {
        weight_lt += from[i].second;
        into[n_lt++] = from[i];
      } else if (pivot < value) {
        into[size - (++n_gt)] = from[i];
      } else {
        weight_eq += from[i].second;
      }
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
    return (lower_middle + upper) / 2;
  }

  return upper;

}

// The weighted forms select an observation whatever LOWER says: a weighted
// median never interpolates, so it is its own lower form.
template <bool NA_RM, bool LOWER = false>
struct median_f;

template <bool LOWER>
struct median_f<false, LOWER> {

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

private:

  std::vector<double> scratch_;
  std::vector< std::pair<double, double> > weighted_scratch_;
  std::vector< std::pair<double, double> > weighted_spare_;

};

template <bool LOWER>
struct median_f<true, LOWER> {

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

private:

  std::vector<double> scratch_;
  std::vector< std::pair<double, double> > weighted_scratch_;
  std::vector< std::pair<double, double> > weighted_spare_;

};

// Sample variance of a window, ignoring NAs. NA when fewer than two values
// remain, matching var()'s behaviour for a vector of length 0 or 1. The first
// pass also reports whether it saw an NA at all, so that the NA-intolerant
// form below does not need a scan of its own.
inline double window_var(double const* x,
                         int offset,
                         int n,
                         bool& has_na) {

  double total = 0.0;
  int count = 0;
  has_na = false;

  for (int i = 0; i < n; ++i) {
    double value = x[offset + i];
    if (is_nan(value)) {
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
    if (!is_nan(value)) {
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
inline double weighted_var(double const* x,
                           int offset,
                           double const* weights,
                           int n,
                           bool& has_na) {

  double weights_sum = 0.0;
  double weighted_total = 0.0;
  int count = 0;
  has_na = false;

  for (int i = 0; i < n; ++i) {
    double value = x[offset + i];
    if (is_nan(value)) {
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
    if (!is_nan(value)) {
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

  inline double operator()(double const* x, int offset, int n) {
    bool has_na;
    double result = window_var(x, offset, n, has_na);
    return has_na ? NA_REAL : result;
  }

  inline double operator()(double const* x,
                           int offset,
                           double const* weights,
                           int n) {
    bool has_na;
    double result = weighted_var(x, offset, weights, n, has_na);
    return has_na ? NA_REAL : result;
  }

};

template <>
struct var_f<true> {

  inline double operator()(double const* x, int offset, int n) {
    bool has_na;
    return window_var(x, offset, n, has_na);
  }

  inline double operator()(double const* x,
                           int offset,
                           double const* weights,
                           int n) {
    bool has_na;
    return weighted_var(x, offset, weights, n, has_na);
  }

};

template <bool NA_RM>
struct sd_f;

template <>
struct sd_f<false> {

  inline double operator()(double const* x, int offset, int n) {
    return window_sqrt(var_f<false>()(x, offset, n));
  }

  inline double operator()(double const* x,
                           int offset,
                           double const* weights,
                           int n) {
    return window_sqrt(var_f<false>()(x, offset, weights, n));
  }

};

template <>
struct sd_f<true> {

  inline double operator()(double const* x, int offset, int n) {
    return window_sqrt(var_f<true>()(x, offset, n));
  }

  inline double operator()(double const* x,
                           int offset,
                           double const* weights,
                           int n) {
    return window_sqrt(var_f<true>()(x, offset, weights, n));
  }

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

// The thread count requested through options(RcppRoll.threads = <n>). Values
// below 1, and a missing option, defer to the OpenMP runtime default, which
// itself respects e.g. OMP_NUM_THREADS. Reads an R option, so this must stay
// on the main thread, outside any parallel region.
inline int requestedThreads() {
  SEXP option = Rf_GetOption1(Rf_install("RcppRoll.threads"));
  if (option != R_NilValue) {
    int threads = Rf_asInteger(option);
    if (threads != NA_INTEGER && threads >= 1)
      return threads;
  }
  return omp_get_max_threads();
}

inline int threadCount(int chunks) {
  if (chunks < 2 || rcpproll_forked)
    return 1;
  int threads = requestedThreads();
  return threads < chunks ? threads : chunks;
}

#endif

// ---------------------------------------------------------------------------
// Drivers
//
// Each writes 'rollOutputSize()' values into a buffer the caller owns, so that
// the matrix routine can hand over a column of its output directly.
// ---------------------------------------------------------------------------

// Walk the clipped windows, writing one value per point.
template <typename Accumulator>
void roll_partial_windows(Accumulator const& prototype,
                          int x_n,
                          double* output,
                          int width,
                          int by,
                          int leftOffset,
                          int rightOffset) {

  int ops = x_n ? (x_n - 1) / by + 1 : 0;
  int chunk = chunkSize(width);
  int chunks = ops ? (ops - 1) / chunk + 1 : 0;

#ifdef _OPENMP
  int threads = threadCount(chunks);
# pragma omp parallel for num_threads(threads) if (threads > 1)
#endif
  for (int c = 0; c < chunks; ++c) {
    Accumulator accumulator(prototype);
    int begin = c * chunk;
    int end = ops - begin > chunk ? begin + chunk : ops;
    for (int j = begin; j < end; ++j) {
      int i = j * by;
      int start = i - leftOffset;
      int stop  = i + rightOffset;
      if (start < 0) start = 0;
      if (stop > x_n - 1) stop = x_n - 1;
      output[i] = accumulator.compute(start, stop);
    }
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
                              char const* align) {

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
  if (Incremental::worthwhile(n, by))
    roll_partial_windows(
      Incremental(f, x, width), x_n, output, width, by,
      leftOffset, rightOffset);
  else
    roll_partial_windows(
      DirectAccumulator<Callable>(f, x, width), x_n, output, width, by,
      leftOffset, rightOffset);

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
                      int padLeftTimes) {

  int ops = to > from ? (to - from - 1) / by + 1 : 0;
  int chunk = chunkSize(n);
  int chunks = ops ? (ops - 1) / chunk + 1 : 0;

#ifdef _OPENMP
  int threads = threadCount(chunks);
# pragma omp parallel for num_threads(threads) if (threads > 1)
#endif
  for (int c = 0; c < chunks; ++c) {
    Accumulator accumulator(prototype);
    int begin = c * chunk;
    int end = ops - begin > chunk ? begin + chunk : ops;
    for (int j = begin; j < end; ++j) {
      int i = from + j * by;
      int start = i - padLeftTimes;
      output[i] = accumulator.compute(start, start + n - 1);
    }
  }

  return from + ops * by;
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
                           char const* align) {

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

  // Fill result -- the driver reports one step past the last window it
  // computed, which is where the 'fill-right' pass-through should start
  int to = padLeftTimes + ops_n;
  if (weights_n) {
    i = roll_fill_windows(
      WeightedAccumulator<Callable>(f, x, weights),
      output, n, by, i, to, padLeftTimes);
  } else {
    typedef typename accumulator_for<Callable>::type Incremental;
    i = Incremental::worthwhile(n, by) ?
      roll_fill_windows(
        Incremental(f, x, n), output, n, by, i, to, padLeftTimes) :
      roll_fill_windows(
        DirectAccumulator<Callable>(f, x, n), output, n, by, i, to,
        padLeftTimes);
  }

  // Fill-right on the remainders. We move the index
  // back one 'by' iteration, then move it back one.
  i -= by;
  ++i;
  for (; i < output_n; ++i)
    output[i] = fill.right();

}

template <typename Accumulator>
void roll_nofill_windows(Accumulator const& prototype,
                         double* output,
                         int n,
                         int by,
                         int output_n) {

  int chunk = chunkSize(n);
  int chunks = output_n ? (output_n - 1) / chunk + 1 : 0;

#ifdef _OPENMP
  int threads = threadCount(chunks);
# pragma omp parallel for num_threads(threads) if (threads > 1)
#endif
  for (int c = 0; c < chunks; ++c) {
    Accumulator accumulator(prototype);
    int begin = c * chunk;
    int end = output_n - begin > chunk ? begin + chunk : output_n;
    for (int i = begin; i < end; ++i) {
      int index = i * by;
      output[i] = accumulator.compute(index, index + n - 1);
    }
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
                             int by) {

  // no complete windows fit, and the output was sized accordingly
  if (x_n < n)
    return;

  int output_n = (x_n - n) / by + 1;

  if (weights_n) {
    roll_nofill_windows(
      WeightedAccumulator<Callable>(f, x, weights), output, n, by, output_n);
  } else {
    typedef typename accumulator_for<Callable>::type Incremental;
    if (Incremental::worthwhile(n, by))
      roll_nofill_windows(Incremental(f, x, n), output, n, by, output_n);
    else
      roll_nofill_windows(
        DirectAccumulator<Callable>(f, x, n), output, n, by, output_n);
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
                      char const* align) {

  // partial windows are computable at every point, so there is nothing to
  // shorten or to pad; 'weights' is rejected upstream in this case
  if (partial)
    roll_vector_partial_into(f, x, x_n, output, n, by, align);
  else if (fill.filled())
    roll_vector_fill_into(
      f, x, x_n, output, n, weights, weights_n, by, fill, align);
  else
    roll_vector_nofill_into(f, x, x_n, output, n, weights, weights_n, by);

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
                      char const* align) {

  SEXP x = PROTECT(Rf_coerceVector(data, REALSXP));
  int x_n = Rf_length(x);

  SEXP output =
    PROTECT(Rf_allocVector(REALSXP, rollOutputSize(x_n, n, by, fill, partial)));

  roll_vector_into(
    f, REAL(x), x_n, REAL(output),
    n, weights, weights_n, by, fill, partial, align);

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
                      char const* align) {

  int nrow = Rf_nrows(data);
  int ncol = Rf_ncols(data);
  int output_nrow = rollOutputSize(nrow, n, by, fill, partial);

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

  for (int j = 0; j < ncol; ++j) {
    roll_vector_into(f, source + (R_xlen_t) j * nrow, nrow,
                     target + (R_xlen_t) j * output_nrow,
                     n, weights, weights_n, by,
                     fill, partial, align);
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
                   char const* align) {

  if (Rf_isMatrix(data))
    return roll_matrix_with(
      f, data, n, weights, weights_n, by, fill, partial, align);

  return roll_vector_with(
    f, data, n, weights, weights_n, by, fill, partial, align);

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

  // uniform weights are an unweighted call in disguise, so route them to the
  // unweighted loops, which carry their windows incrementally where the
  // weighted forms recompute every window
  if (weightsAreUniform(REAL(weights), weights_n, normalize))
    return roll_dispatch(
      uniform_equivalent(f), data, n,
      (double const*) NULL, 0, by, fill, partial, align);

  std::vector<double> scaled =
    normalizeWeights(REAL(weights), weights_n, n, normalize);
  double const* weights_data = scaled.empty() ? NULL : &scaled[0];

  return roll_dispatch(
    f, data, n, weights_data, weights_n, by, fill, partial, align);

}

}  // end namespace RcppRoll

extern "C" SEXP na_locf(SEXP x)
{
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
