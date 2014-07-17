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

  if (normalize && weights.size())
    weights = weights / sum(weights) * n;

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

  // figure out if we need to pad at the start, end, etc.
  int padLeftTimes  = getLeftPadding(fill, align, n);
  int padRightTimes = getRightPadding(fill, align, n);

  int x_n = x.size();
  int ops_n = x_n - n + 1;
  int output_n = padLeftTimes + ops_n + padRightTimes;

  T result;
  if (by > 1) {
    result = static_cast<T>(no_init(output_n));
  } else {
    result = T(output_n, fill.middle());
  }

  // pad left
  for (int i = 0; i < padLeftTimes; ++i) {
    result[i] = fill.left();
  }

  // fill result
  if (weights.size()) {
    for (int i = padLeftTimes; i < padLeftTimes + ops_n; i += by) {
      result[i] = f(x, i - padLeftTimes, weights, n);
    }
  } else {
    for (int i = padLeftTimes; i < padLeftTimes + ops_n; i += by) {
      result[i] = f(x, i - padLeftTimes, n);
    }
  }

  // pad right
  for (int i = padLeftTimes + ops_n; i < padLeftTimes + ops_n + padRightTimes; ++i) {
    result[i] = fill.right();
  }

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
  int ops_n = x_n - n + 1;
  int output_n = ops_n;

  T result;
  if (by > 1) {
    result = static_cast<T>(no_init(output_n));
  } else {
    result = T(output_n, fill.middle());
  }

  // fill result
  if (weights.size()) {
    for (int i = 0; i < ops_n; i += by) {
      result[i] = f(x, i, weights, n);
    }
  } else {
    for (int i = 0; i < ops_n; i += by) {
      result[i] = f(x, i, n);
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

struct mean_f {

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

struct sum_f {

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

struct min_f {

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

struct max_f {

  inline double operator()(NumericVector const& x,
                           int offset,
                           NumericVector& weights,
                           int n) {
    double result = R_NegInf;
    for (int i = 0; i < n; ++i) {
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
      result = x[offset + i] < result ? result : x[offset + i];
    }
    return result;
  }
};

struct prod_f {

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

std::vector<double> square(std::vector<double> const& x, int exponent) {
  std::vector<double> result;
  result.reserve(x.size());
  std::transform(
      x.begin(), x.begin(), result.begin(), [](double x) { return x * x; });
  return result;
}

struct median_f {

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

struct var_f {

  inline double operator()(NumericVector const& x, int offset, int n) {
    return var(NumericVector(x.begin() + offset, x.begin() + offset + n));
  }

  inline double operator()(NumericVector const& x, int offset, NumericVector weights, int n) {
    NumericVector sub(x.begin() + offset, x.begin() + offset + n);
    return var(sub * weights);
  }

};

struct sd_f {

  inline double operator()(NumericVector const& x, int offset, int n) {
    return sqrt(var(NumericVector(x.begin() + offset, x.begin() + offset + n)));
  }

  inline double operator()(NumericVector const& x, int offset, NumericVector weights, int n) {
    NumericVector sub(x.begin() + offset, x.begin() + offset + n);
    return sqrt(var(sub * weights));
  }

};

}  // end namespace RcppRoll

// Begin auto-generated exports (internal/make_exports.R)

// [[Rcpp::export(.RcppRoll_mean)]]
SEXP roll_mean(SEXP x, int n, NumericVector weights,
  int by, NumericVector fill_, bool partial, String align, bool normalize) {

  RcppRoll::Fill fill(fill_);
  if (Rf_isMatrix(x)) {
    return RcppRoll::roll_matrix_with(
        RcppRoll::mean_f(), NumericMatrix(x), n, weights, by, fill, partial, align, normalize);
  } else {
    return RcppRoll::roll_vector_with(
        RcppRoll::mean_f(), NumericVector(x), n, weights, by, fill, partial, align, normalize);
  }

}

// [[Rcpp::export(.RcppRoll_median)]]
SEXP roll_median(SEXP x, int n, NumericVector weights,
  int by, NumericVector fill_, bool partial, String align, bool normalize) {

  RcppRoll::Fill fill(fill_);
  if (Rf_isMatrix(x)) {
    return RcppRoll::roll_matrix_with(
        RcppRoll::median_f(), NumericMatrix(x), n, weights, by, fill, partial, align, normalize);
  } else {
    return RcppRoll::roll_vector_with(
        RcppRoll::median_f(), NumericVector(x), n, weights, by, fill, partial, align, normalize);
  }

}

// [[Rcpp::export(.RcppRoll_min)]]
SEXP roll_min(SEXP x, int n, NumericVector weights,
  int by, NumericVector fill_, bool partial, String align, bool normalize) {

  RcppRoll::Fill fill(fill_);
  if (Rf_isMatrix(x)) {
    return RcppRoll::roll_matrix_with(
        RcppRoll::min_f(), NumericMatrix(x), n, weights, by, fill, partial, align, normalize);
  } else {
    return RcppRoll::roll_vector_with(
        RcppRoll::min_f(), NumericVector(x), n, weights, by, fill, partial, align, normalize);
  }

}

// [[Rcpp::export(.RcppRoll_max)]]
SEXP roll_max(SEXP x, int n, NumericVector weights,
  int by, NumericVector fill_, bool partial, String align, bool normalize) {

  RcppRoll::Fill fill(fill_);
  if (Rf_isMatrix(x)) {
    return RcppRoll::roll_matrix_with(
        RcppRoll::max_f(), NumericMatrix(x), n, weights, by, fill, partial, align, normalize);
  } else {
    return RcppRoll::roll_vector_with(
        RcppRoll::max_f(), NumericVector(x), n, weights, by, fill, partial, align, normalize);
  }

}

// [[Rcpp::export(.RcppRoll_prod)]]
SEXP roll_prod(SEXP x, int n, NumericVector weights,
  int by, NumericVector fill_, bool partial, String align, bool normalize) {

  RcppRoll::Fill fill(fill_);
  if (Rf_isMatrix(x)) {
    return RcppRoll::roll_matrix_with(
        RcppRoll::prod_f(), NumericMatrix(x), n, weights, by, fill, partial, align, normalize);
  } else {
    return RcppRoll::roll_vector_with(
        RcppRoll::prod_f(), NumericVector(x), n, weights, by, fill, partial, align, normalize);
  }

}

// [[Rcpp::export(.RcppRoll_sum)]]
SEXP roll_sum(SEXP x, int n, NumericVector weights,
  int by, NumericVector fill_, bool partial, String align, bool normalize) {

  RcppRoll::Fill fill(fill_);
  if (Rf_isMatrix(x)) {
    return RcppRoll::roll_matrix_with(
        RcppRoll::sum_f(), NumericMatrix(x), n, weights, by, fill, partial, align, normalize);
  } else {
    return RcppRoll::roll_vector_with(
        RcppRoll::sum_f(), NumericVector(x), n, weights, by, fill, partial, align, normalize);
  }

}

// [[Rcpp::export(.RcppRoll_sd)]]
SEXP roll_sd(SEXP x, int n, NumericVector weights,
  int by, NumericVector fill_, bool partial, String align, bool normalize) {

  RcppRoll::Fill fill(fill_);
  if (Rf_isMatrix(x)) {
    return RcppRoll::roll_matrix_with(
        RcppRoll::sd_f(), NumericMatrix(x), n, weights, by, fill, partial, align, normalize);
  } else {
    return RcppRoll::roll_vector_with(
        RcppRoll::sd_f(), NumericVector(x), n, weights, by, fill, partial, align, normalize);
  }

}

// [[Rcpp::export(.RcppRoll_var)]]
SEXP roll_var(SEXP x, int n, NumericVector weights,
  int by, NumericVector fill_, bool partial, String align, bool normalize) {

  RcppRoll::Fill fill(fill_);
  if (Rf_isMatrix(x)) {
    return RcppRoll::roll_matrix_with(
        RcppRoll::var_f(), NumericMatrix(x), n, weights, by, fill, partial, align, normalize);
  } else {
    return RcppRoll::roll_vector_with(
        RcppRoll::var_f(), NumericVector(x), n, weights, by, fill, partial, align, normalize);
  }

}

// End auto-generated exports (internal/make_exports.R)
