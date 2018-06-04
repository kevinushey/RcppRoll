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

inline int getLeftPadding(Fill const& fill, String const& align, int n) {
  if (!fill.filled()) return 0;
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

inline int getRightPadding(Fill const& fill, String const& align, int n) {
  if (!fill.filled()) return 0;
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
  
  
  

  if (normalize && weights.size()){
    // Normalize 'n' to match that of weights
    if (weights.size()){
      n = weights.size();
    }
    
    //Create a local copy of the weight vectors
    NumericVector weights_normalized = weights / sum(weights) * n;
    
    return fill.filled() ?
    roll_vector_with_fill(f, x, n, weights_normalized, by, fill, partial, align) :
    roll_vector_with_nofill(f, x, n, weights_normalized, by, fill, partial, align)
  ;
  }
  
  return fill.filled() ?
    roll_vector_with_fill(f, x, n, weights, by, fill, partial, align) :
    roll_vector_with_nofill(f, x, n, weights, by, fill, partial, align)
  ;

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
  if (fill.filled()) {
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
    double result = 0.0;
    int num = 0;
    for (int i = 0; i < n; ++i) {
      if (!ISNAN(x[offset + i])) {
        result += x[offset + i] * weights[i];
        ++num;
      }
    }
    return result / num;
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

template <bool NA_RM>
struct median_f;

template <>
struct median_f<false> {

  inline double operator()(NumericVector const& x, int offset, int n) {

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

    NumericVector copy(x.begin() + offset, x.begin() + offset + n);
    std::sort(copy.begin(), copy.end());

    double weights_sum = sum(weights);

    int k = 0;
    double sum = weights_sum - weights[0];

    while (sum > weights_sum / 2) {
      ++k;
      sum -= weights[k];
    }

    return copy[k];
  }

};

template <>
struct median_f<true> {

  inline double operator()(NumericVector const& x, int offset, int n) {

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

    NumericVector copy(x.begin() + offset, x.begin() + offset + n);
    std::sort(copy.begin(), copy.end());

    double weights_sum = sum(weights);

    int k = 0;
    double sum = weights_sum - weights[0];

    while (sum > weights_sum / 2) {
      ++k;
      sum -= weights[k];
    }

    return copy[k];
  }

};

template <bool NA_RM>
struct var_f;

template <>
struct var_f<false> {

  inline double operator()(NumericVector const& x, int offset, int n) {
    return var(NumericVector(x.begin() + offset, x.begin() + offset + n));
  }

  inline double operator()(NumericVector const& x, int offset, NumericVector weights, int n) {
    NumericVector sub(x.begin() + offset, x.begin() + offset + n);
    return var(sub * weights);
  }

};

template <>
struct var_f<true> {

  inline double operator()(NumericVector const& x, int offset, int n) {
    NumericVector sub(x.begin() + offset, x.begin() + offset + n);
    sub = na_omit(sub);
    return var(sub);
  }

  inline double operator()(NumericVector const& x, int offset, NumericVector weights, int n) {
    NumericVector sub(x.begin() + offset, x.begin() + offset + n);
    sub = na_omit(sub);
    return var(sub * weights);
  }

};

template <bool NA_RM>
struct sd_f;

template <>
struct sd_f<false> {

  inline double operator()(NumericVector const& x, int offset, int n) {
    return sqrt(var(NumericVector(x.begin() + offset, x.begin() + offset + n)));
  }

  inline double operator()(NumericVector const& x, int offset, NumericVector weights, int n) {
    NumericVector sub(x.begin() + offset, x.begin() + offset + n);
    return sqrt(var(sub * weights));
  }

};

template <>
struct sd_f<true> {

  inline double operator()(NumericVector const& x, int offset, int n) {
    NumericVector sub(x.begin() + offset, x.begin() + offset + n);
    sub = na_omit(sub);
    return sqrt(var(sub));
  }

  inline double operator()(NumericVector const& x, int offset, NumericVector weights, int n) {
    NumericVector sub(x.begin() + offset, x.begin() + offset + n);
    sub = na_omit(sub);
    return sqrt(var(sub * weights));
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

// [[Rcpp::export(.RcppRoll_mean)]]
SEXP roll_mean(SEXP x,
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
// [[Rcpp::export(.RcppRoll_median)]]
SEXP roll_median(SEXP x,
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
// [[Rcpp::export(.RcppRoll_min)]]
SEXP roll_min(SEXP x,
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
// [[Rcpp::export(.RcppRoll_max)]]
SEXP roll_max(SEXP x,
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
// [[Rcpp::export(.RcppRoll_prod)]]
SEXP roll_prod(SEXP x,
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
// [[Rcpp::export(.RcppRoll_sum)]]
SEXP roll_sum(SEXP x,
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
// [[Rcpp::export(.RcppRoll_sd)]]
SEXP roll_sd(SEXP x,
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
// [[Rcpp::export(.RcppRoll_var)]]
SEXP roll_var(SEXP x,
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
