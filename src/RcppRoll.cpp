#define DEBUG(x) x

#include <Rcpp.h>
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

template <typename T>
struct product {
  inline T operator()(T const& left, T const& right) { return left * right; }
};

template <typename T>
inline double prod(T const& x) {
  return std::accumulate(x.begin(), x.end(), 1.0, product<double>());
}

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

template <typename Callable, typename T>
T roll_vector_with(Callable f,
                   T const& x,
                   int n,
                   NumericVector& weights,
                   int by,
                   Fill const& fill,
                   bool partial,
                   String const& align,
                   bool normalize) {

  // Normalize 'n' to match that of weights
  if (weights.size())
    n = weights.size();

  if (normalize && weights.size())
    weights = Rcpp::clone(NumericVector(weights / sum(weights) * n));

  // partial windows are computable at every point, so there is nothing to
  // shorten or to pad; 'weights' is rejected upstream in this case
  if (partial)
    return roll_vector_with_partial(f, x, n, by, fill, align);

  return fill.filled() ?
    roll_vector_with_fill(f, x, n, weights, by, fill, partial, align) :
    roll_vector_with_nofill(f, x, n, weights, by, fill, partial, align)
  ;

}

// Windows clipped to the bounds of 'x': every point gets an answer, computed
// over however many observations are in range. The point a window is reported
// at is always in range itself, so a window is never empty.
template <typename Callable, typename T>
T roll_vector_with_partial(Callable f,
                           T const& x,
                           int n,
                           int by,
                           Fill const& fill,
                           String const& align) {

  int x_n = x.size();
  int leftOffset  = getLeftOffset(align, n);
  int rightOffset = getRightOffset(align, n);

  T result;
  if (by == 1) {
    result = static_cast<T>(no_init(x_n));
  } else {
    // points we skip over are not computed, and 'fill' does not apply here
    result = T(x_n, T::get_na());
  }

  for (int i = 0; i < x_n; i += by) {
    int start = i - leftOffset;
    int end   = i + rightOffset;
    if (start < 0) start = 0;
    if (end > x_n - 1) end = x_n - 1;
    result[i] = f(x, start, end - start + 1);
  }

  return result;
}

template <typename Callable, typename T>
T roll_vector_with_fill(Callable f,
                   T const& x,
                   int n,
                   NumericVector& weights,
                   int by,
                   Fill const& fill,
                   bool partial,
                   String const& align) {

  if (x.size() < n)
    return rep(T::get_na(), x.size());

  // figure out if we need to pad at the start, end, etc.
  int padLeftTimes  = getLeftPadding(fill, align, n);
  int padRightTimes = getRightPadding(fill, align, n);

  int x_n = x.size();
  int ops_n = x_n - n + 1;
  int output_n = padLeftTimes + ops_n + padRightTimes;

  T result;
  int i = 0;

  if (by == 1) {
    result = static_cast<T>(no_init(output_n));
  } else {
    result = T(output_n, fill.middle());
  }

  // Pad left
  for (; i < padLeftTimes; ++i)
    result[i] = fill.left();

  // Fill result -- we hoist the indexing variable outside of the loop
  // so we can re-use it to easily figure out where our 'fill-right'
  // pass-through should start
  if (weights.size()) {
    for (; i < padLeftTimes + ops_n; i += by) {
      result[i] = f(x, i - padLeftTimes, weights, n);
    }
  } else {
    for (; i < padLeftTimes + ops_n; i += by) {
      result[i] = f(x, i - padLeftTimes, n);
    }
  }

  // Fill-right on the remainders. We move the index
  // back one 'by' iteration, then move it back one.
  i -= by;
  ++i;
  for (; i < output_n; ++i)
    result[i] = fill.right();

  return result;
}

template <typename Callable, typename T>
T roll_vector_with_nofill(Callable f,
                          T const& x,
                          int n,
                          NumericVector& weights,
                          int by,
                          Fill const& fill,
                          bool partial,
                          String const& align) {

  int x_n = x.size();
  int output_n = (x_n - n) / by + 1;

  T result = static_cast<T>(no_init(output_n));

  int index = 0;
  if (weights.size()) {
    for (int i = 0; i < output_n; ++i) {
      result[i] = f(x, index, weights, n);
      index += by;
    }
  } else {
    for (int i = 0; i < output_n; ++i) {
      result[i] = f(x, index, n);
      index += by;
    }
  }

  return result;
}

template <typename Callable, typename T>
T roll_matrix_with(Callable f,
                   T const& x,
                   int n,
                   NumericVector& weights,
                   int by,
                   Fill const& fill,
                   bool partial,
                   String const& align,
                   bool normalize) {

  int nrow = x.nrow();
  int ncol = x.ncol();

  T output;
  if (partial || fill.filled()) {
    output = T(nrow, ncol);
  } else {
    output = T(nrow - n + 1, ncol);
  }

  for (int i = 0; i < ncol; ++i) {
    output(_, i) = roll_vector_with(
      f, static_cast<NumericVector>(x(_, i)), n, weights, by,
        fill, partial, align, normalize);
  }

  return output;
}

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
                           NumericVector& weights,
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
                           NumericVector& weights,
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
                           NumericVector& weights,
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
                           NumericVector& weights,
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
                           NumericVector& weights,
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
                           NumericVector& weights,
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
                           NumericVector& weights,
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
                           NumericVector& weights,
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
                           NumericVector& weights,
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
                           NumericVector& weights,
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
inline double weighted_median(NumericVector const& x,
                              int offset,
                              NumericVector const& weights,
                              int n) {

  std::vector< std::pair<double, double> > pairs;
  pairs.reserve(n);
  for (int i = 0; i < n; ++i) {
    double value = x[offset + i];
    if (!ISNAN(value))
      pairs.push_back(std::make_pair(value, weights[i]));
  }

  if (pairs.empty())
    return NA_REAL;

  std::sort(pairs.begin(), pairs.end());

  double weights_sum = 0.0;
  for (size_t i = 0; i < pairs.size(); ++i)
    weights_sum += pairs[i].second;

  // guard against zero, negative, or non-finite weight sums, which would
  // otherwise let the search below run past the end of the window
  if (!(weights_sum > 0))
    return NA_REAL;

  size_t k = 0;
  double remaining = weights_sum;
  for (; k + 1 < pairs.size(); ++k) {
    remaining -= pairs[k].second;
    if (!(remaining > weights_sum / 2))
      break;
  }

  return pairs[k].first;

}

template <bool NA_RM>
struct median_f;

template <>
struct median_f<false> {

  inline double operator()(NumericVector const& x, int offset, int n) {

    for (int i = offset; i < offset + n; i++)
      if (ISNAN(x[i]))
        return NA_REAL;

    std::vector<double> copied(n / 2 + 1);

    std::partial_sort_copy(
      x.begin() + offset,
      x.begin() + offset + n,
      copied.begin(),
      copied.begin() + n / 2 + 1
    );

    if (n % 2 == 0) {
      return (copied[n / 2 - 1] + copied[n / 2]) / 2;
    } else {
      return copied[n / 2];
    }

  }

  inline double operator()(NumericVector const& x,
                           int offset,
                           NumericVector& weights,
                           int n) {

    for (int i = offset; i < offset + n; i++)
      if (ISNAN(x[i]))
        return NA_REAL;

    return weighted_median(x, offset, weights, n);
  }

};

template <>
struct median_f<true> {

  inline double operator()(NumericVector const& x, int offset, int n) {

    std::vector<double> data;
    for (int i = offset; i < offset + n; i++)
      if (!ISNAN(x[i]))
        data.push_back(x[i]);

    n = data.size();
    if (n == 0)
      return NA_REAL;

    std::vector<double> copied(n / 2 + 1);

    std::partial_sort_copy(
      data.begin(),   data.end(),
      copied.begin(), copied.end()
    );

    if (n % 2 == 0) {
      return (copied[n / 2 - 1] + copied[n / 2]) / 2;
    } else {
      return copied[n / 2];
    }

  }

  inline double operator()(NumericVector const& x,
                           int offset,
                           NumericVector& weights,
                           int n) {

    return weighted_median(x, offset, weights, n);
  }

};

// Sample variance of a window, ignoring NAs. NA when fewer than two values
// remain, matching var()'s behaviour for a vector of length 0 or 1.
inline double window_var(NumericVector const& x, int offset, int n) {

  double total = 0.0;
  int count = 0;
  for (int i = 0; i < n; ++i) {
    double value = x[offset + i];
    if (!ISNAN(value)) {
      total += value;
      ++count;
    }
  }

  if (count < 2)
    return NA_REAL;

  double mean = total / count;

  double result = 0.0;
  for (int i = 0; i < n; ++i) {
    double value = x[offset + i];
    if (!ISNAN(value)) {
      double difference = value - mean;
      result += difference * difference;
    }
  }

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
                           int n) {

  double weights_sum = 0.0;
  double weighted_total = 0.0;
  int count = 0;

  for (int i = 0; i < n; ++i) {
    double value = x[offset + i];
    if (!ISNAN(value)) {
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

  double result = 0.0;
  for (int i = 0; i < n; ++i) {
    double value = x[offset + i];
    if (!ISNAN(value)) {
      double difference = value - mean;
      result += weights[i] * difference * difference;
    }
  }

  return result / (weights_sum - 1);

}

inline bool window_has_na(NumericVector const& x, int offset, int n) {
  for (int i = offset; i < offset + n; ++i)
    if (ISNAN(x[i]))
      return true;
  return false;
}

// sqrt() would turn NA_REAL into a plain NaN, so pass non-values through
inline double window_sqrt(double value) {
  return ISNAN(value) ? value : sqrt(value);
}

template <bool NA_RM>
struct var_f;

template <>
struct var_f<false> {

  inline double operator()(NumericVector const& x, int offset, int n) {
    if (window_has_na(x, offset, n))
      return NA_REAL;
    return window_var(x, offset, n);
  }

  inline double operator()(NumericVector const& x, int offset, NumericVector weights, int n) {
    if (window_has_na(x, offset, n))
      return NA_REAL;
    return weighted_var(x, offset, weights, n);
  }

};

template <>
struct var_f<true> {

  inline double operator()(NumericVector const& x, int offset, int n) {
    return window_var(x, offset, n);
  }

  inline double operator()(NumericVector const& x, int offset, NumericVector weights, int n) {
    return weighted_var(x, offset, weights, n);
  }

};

template <bool NA_RM>
struct sd_f;

template <>
struct sd_f<false> {

  inline double operator()(NumericVector const& x, int offset, int n) {
    return window_sqrt(var_f<false>()(x, offset, n));
  }

  inline double operator()(NumericVector const& x, int offset, NumericVector weights, int n) {
    return window_sqrt(var_f<false>()(x, offset, weights, n));
  }

};

template <>
struct sd_f<true> {

  inline double operator()(NumericVector const& x, int offset, int n) {
    return window_sqrt(var_f<true>()(x, offset, n));
  }

  inline double operator()(NumericVector const& x, int offset, NumericVector weights, int n) {
    return window_sqrt(var_f<true>()(x, offset, weights, n));
  }

};

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
  if (Rf_isMatrix(x)) {
    if (na_rm) {
      return RcppRoll::roll_matrix_with(
        RcppRoll::mean_f<true>(), NumericMatrix(x), n, weights, by, fill, partial, align, normalize);
    } else {
      return RcppRoll::roll_matrix_with(
        RcppRoll::mean_f<false>(), NumericMatrix(x), n, weights, by, fill, partial, align, normalize);
    }
  } else {
    if (na_rm) {
      return RcppRoll::roll_vector_with(
        RcppRoll::mean_f<true>(), NumericVector(x), n, weights, by, fill, partial, align, normalize);
    } else {
      return RcppRoll::roll_vector_with(
        RcppRoll::mean_f<false>(), NumericVector(x), n, weights, by, fill, partial, align, normalize);
    }
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
  if (Rf_isMatrix(x)) {
    if (na_rm) {
      return RcppRoll::roll_matrix_with(
        RcppRoll::median_f<true>(), NumericMatrix(x), n, weights, by, fill, partial, align, normalize);
    } else {
      return RcppRoll::roll_matrix_with(
        RcppRoll::median_f<false>(), NumericMatrix(x), n, weights, by, fill, partial, align, normalize);
    }
  } else {
    if (na_rm) {
      return RcppRoll::roll_vector_with(
        RcppRoll::median_f<true>(), NumericVector(x), n, weights, by, fill, partial, align, normalize);
    } else {
      return RcppRoll::roll_vector_with(
        RcppRoll::median_f<false>(), NumericVector(x), n, weights, by, fill, partial, align, normalize);
    }
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
  if (Rf_isMatrix(x)) {
    if (na_rm) {
      return RcppRoll::roll_matrix_with(
        RcppRoll::min_f<true>(), NumericMatrix(x), n, weights, by, fill, partial, align, normalize);
    } else {
      return RcppRoll::roll_matrix_with(
        RcppRoll::min_f<false>(), NumericMatrix(x), n, weights, by, fill, partial, align, normalize);
    }
  } else {
    if (na_rm) {
      return RcppRoll::roll_vector_with(
        RcppRoll::min_f<true>(), NumericVector(x), n, weights, by, fill, partial, align, normalize);
    } else {
      return RcppRoll::roll_vector_with(
        RcppRoll::min_f<false>(), NumericVector(x), n, weights, by, fill, partial, align, normalize);
    }
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
  if (Rf_isMatrix(x)) {
    if (na_rm) {
      return RcppRoll::roll_matrix_with(
        RcppRoll::max_f<true>(), NumericMatrix(x), n, weights, by, fill, partial, align, normalize);
    } else {
      return RcppRoll::roll_matrix_with(
        RcppRoll::max_f<false>(), NumericMatrix(x), n, weights, by, fill, partial, align, normalize);
    }
  } else {
    if (na_rm) {
      return RcppRoll::roll_vector_with(
        RcppRoll::max_f<true>(), NumericVector(x), n, weights, by, fill, partial, align, normalize);
    } else {
      return RcppRoll::roll_vector_with(
        RcppRoll::max_f<false>(), NumericVector(x), n, weights, by, fill, partial, align, normalize);
    }
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
  if (Rf_isMatrix(x)) {
    if (na_rm) {
      return RcppRoll::roll_matrix_with(
        RcppRoll::prod_f<true>(), NumericMatrix(x), n, weights, by, fill, partial, align, normalize);
    } else {
      return RcppRoll::roll_matrix_with(
        RcppRoll::prod_f<false>(), NumericMatrix(x), n, weights, by, fill, partial, align, normalize);
    }
  } else {
    if (na_rm) {
      return RcppRoll::roll_vector_with(
        RcppRoll::prod_f<true>(), NumericVector(x), n, weights, by, fill, partial, align, normalize);
    } else {
      return RcppRoll::roll_vector_with(
        RcppRoll::prod_f<false>(), NumericVector(x), n, weights, by, fill, partial, align, normalize);
    }
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
  if (Rf_isMatrix(x)) {
    if (na_rm) {
      return RcppRoll::roll_matrix_with(
        RcppRoll::sum_f<true>(), NumericMatrix(x), n, weights, by, fill, partial, align, normalize);
    } else {
      return RcppRoll::roll_matrix_with(
        RcppRoll::sum_f<false>(), NumericMatrix(x), n, weights, by, fill, partial, align, normalize);
    }
  } else {
    if (na_rm) {
      return RcppRoll::roll_vector_with(
        RcppRoll::sum_f<true>(), NumericVector(x), n, weights, by, fill, partial, align, normalize);
    } else {
      return RcppRoll::roll_vector_with(
        RcppRoll::sum_f<false>(), NumericVector(x), n, weights, by, fill, partial, align, normalize);
    }
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
  if (Rf_isMatrix(x)) {
    if (na_rm) {
      return RcppRoll::roll_matrix_with(
        RcppRoll::sd_f<true>(), NumericMatrix(x), n, weights, by, fill, partial, align, normalize);
    } else {
      return RcppRoll::roll_matrix_with(
        RcppRoll::sd_f<false>(), NumericMatrix(x), n, weights, by, fill, partial, align, normalize);
    }
  } else {
    if (na_rm) {
      return RcppRoll::roll_vector_with(
        RcppRoll::sd_f<true>(), NumericVector(x), n, weights, by, fill, partial, align, normalize);
    } else {
      return RcppRoll::roll_vector_with(
        RcppRoll::sd_f<false>(), NumericVector(x), n, weights, by, fill, partial, align, normalize);
    }
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
  if (Rf_isMatrix(x)) {
    if (na_rm) {
      return RcppRoll::roll_matrix_with(
        RcppRoll::var_f<true>(), NumericMatrix(x), n, weights, by, fill, partial, align, normalize);
    } else {
      return RcppRoll::roll_matrix_with(
        RcppRoll::var_f<false>(), NumericMatrix(x), n, weights, by, fill, partial, align, normalize);
    }
  } else {
    if (na_rm) {
      return RcppRoll::roll_vector_with(
        RcppRoll::var_f<true>(), NumericVector(x), n, weights, by, fill, partial, align, normalize);
    } else {
      return RcppRoll::roll_vector_with(
        RcppRoll::var_f<false>(), NumericVector(x), n, weights, by, fill, partial, align, normalize);
    }
  }
}
// End auto-generated exports (internal/make-exports.R)
