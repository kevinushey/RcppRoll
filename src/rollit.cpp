#include <Rcpp.h>
using namespace Rcpp;

namespace RcppRoll {

template <typename T>
struct product {
  inline T operator()(T const& left, T const& right) { return left * right; }
};

template <typename T>
inline double prod(T const& x) {
  return std::accumulate(x.begin(), x.end(), 1.0, product<double>());
}

template <typename Callable, typename T>
T roll_vector_with(Callable f,
                   T const& x,
                   SEXP weights,
                   int n,
                   SEXP pad,
                   bool normalize) {

#define DISPATCH(__X__)                           \
  if (Rf_isNull(weights)) {                       \
    return __X__(f, x, n);                        \
  } else {                                        \
    if (normalize) {                              \
      NumericVector w(weights);                   \
      return __X__(f, x, w / sum(w), n);             \
    } else {                                      \
      return __X__(f, x, NumericVector(weights), n); \
    }                                             \
  }

  if (Rf_isNull(pad)) {
    DISPATCH(roll_vector_with_nopad);
  } else if (strcmp(CHAR(STRING_ELT(pad, 0)), "left") == 0) {
    DISPATCH(roll_vector_with_pad_left);
  } else if (strcmp(CHAR(STRING_ELT(pad, 0)), "right") == 0) {
    DISPATCH(roll_vector_with_pad_right);
  } else {
    DISPATCH(roll_vector_with_nopad);
  }

#undef DISPATCH
}

// no padding + weights
template <typename Callable, typename T>
T roll_vector_with_nopad(Callable f, T const& x, NumericVector const& weights, int weights_n) {

  int x_n = x.size();
  int n_ops = x_n - weights_n + 1;

  T result = no_init(n_ops);
  for (int i = 0; i < n_ops; ++i) {
    result[i] = f(x, i, weights, weights_n);
  }

  return result;
}

// no padding + unit weights
template <typename Callable, typename T>
T roll_vector_with_nopad(Callable f, T const& x, int n) {

  int x_n = x.size();
  int n_ops = x_n - n + 1;

  T result = no_init(n_ops);
  for (int i = 0; i < n_ops; ++i) {
    result[i] = f(x, i, n);
  }

  return result;
}

// pad left + weights
template <typename Callable, typename T>
T roll_vector_with_pad_left(Callable f, T const& x, NumericVector weights, int weights_n) {

  int x_n = x.size();
  int n_ops = x_n - weights_n + 1;

  T result = no_init(x_n);
  for (int i = 0; i < weights_n - 1; ++i) {
    result[i] = T::get_na();
  }
  for (int i = 0; i < n_ops; ++i) {
    result[i + weights_n - 1] = f(x, i, weights, weights_n);
  }

  return result;
}

// pad left, unit weights
template <typename Callable, typename T>
T roll_vector_with_pad_left(Callable f, T const& x, int n) {

  int x_n = x.size();
  int n_ops = x_n - n + 1;

  T result = no_init(x_n);
  for (int i = 0; i < n - 1; ++i) {
    result[i] = T::get_na();
  }
  for (int i = 0; i < n_ops; ++i) {
    result[i + n - 1] = f(x, i, n);
  }

  return result;
}

// pad right + weights
template <typename Callable, typename T>
T roll_vector_with_pad_right(Callable f, T const& x, NumericVector weights, int weights_n) {

  int x_n = x.size();

  T result = no_init(x_n);
  for (int i = 0; i < x_n - weights_n + 1; ++i) {
    result[i] = f(x, i, weights, weights_n);
  }
  for (int i = x_n - weights_n + 1; i < x_n; ++i) {
    result[i] = T::get_na();
  }

  return result;
}

// pad right + unit weights
template <typename Callable, typename T>
T roll_vector_with_pad_right(Callable f, T const& x, int n) {

  int x_n = x.size();

  T result = no_init(x_n);

  for (int i = 0; i < x_n - n + 1; ++i) {
    result[i] = f(x, i, n);
  }
  for (int i = x_n - n + 1; i < x_n; ++i) {
    result[i] = T::get_na();
  }

  return result;
}

template <typename Callable, typename T>
T roll_matrix_with(Callable f,
                   T const& x,
                   SEXP weights,
                   int n,
                   SEXP pad,
                   bool normalize) {

  int nrow = x.nrow();
  int ncol = x.ncol();

  T output(nrow - n + 1, ncol);

  for (int i = 0; i < ncol; ++i) {
    output(_, i) = roll_vector_with(
        f, static_cast<NumericVector>(x(_, i)), weights, n, pad, normalize);
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
                           NumericVector const& weights,
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
                           NumericVector const& weights,
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
      result = x[i] < result ? x[i] : result;
    }
    return result;
  }
};

struct max_f {

  inline double operator()(NumericVector const& x,
                           int offset,
                           NumericVector const& weights,
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
      result = x[i] < result ? result : x[i];
    }
    return result;
  }
};

struct prod_f {

  inline double operator()(NumericVector const& x, int offset, int n) {
    double result = 0.0;
    for (int i = 0; i < n; ++i) {
      result *= x[offset + i];
    }
    return result;
  }

  inline double operator()(NumericVector const& x,
                           int offset,
                           NumericVector const& weights,
                           int n) {
    double result = 0.0;
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

    int x_n = x.size();

    // if 'x' has even length, we want the two middle elements
    int x_n_half = (x_n + 1) / 2;

    std::vector<double> copied;
    copied.reserve(x_n_half);

    std::partial_sort_copy(x.begin(), x.end(), copied.begin(), copied.begin() + x_n_half + 1);
    if (n % 2 == 0) {
      return (copied[x_n_half - 1] + copied[x_n_half]) / 2;
    } else {
      return copied[x_n_half];
    }

  }

  inline double operator()(NumericVector const& x,
                           int offset,
                           NumericVector const& weights,
                           int n) {

    NumericVector copy = clone(x);
    std::sort(copy.begin(), copy.end());

    double weights_sum = sum(weights);

    int k = 0;
    double sum = weights_sum - weights[0];

    while (sum > weights_sum / 2) {
      ++k;
      sum -= weights[k];
    }

    return x[k];
  }

};

}  // end namespace RcppRoll

// Begin auto-generated exports (internal/make_exports.R)

// [[Rcpp::export(.RcppRoll_mean)]]
SEXP roll_mean(SEXP x, int n, SEXP weights, SEXP pad, bool by_column, bool normalize) {

  if (Rf_isMatrix(x)) {
    return RcppRoll::roll_matrix_with(
        RcppRoll::mean_f(), NumericMatrix(x), weights, n, pad, normalize);
  } else {
    return RcppRoll::roll_vector_with(
        RcppRoll::mean_f(), NumericVector(x), weights, n, pad, normalize);
  }

}

// [[Rcpp::export(.RcppRoll_median)]]
SEXP roll_median(SEXP x, int n, SEXP weights, SEXP pad, bool by_column, bool normalize) {

  if (Rf_isMatrix(x)) {
    return RcppRoll::roll_matrix_with(
        RcppRoll::median_f(), NumericMatrix(x), weights, n, pad, normalize);
  } else {
    return RcppRoll::roll_vector_with(
        RcppRoll::median_f(), NumericVector(x), weights, n, pad, normalize);
  }

}

// [[Rcpp::export(.RcppRoll_min)]]
SEXP roll_min(SEXP x, int n, SEXP weights, SEXP pad, bool by_column, bool normalize) {

  if (Rf_isMatrix(x)) {
    return RcppRoll::roll_matrix_with(
        RcppRoll::min_f(), NumericMatrix(x), weights, n, pad, normalize);
  } else {
    return RcppRoll::roll_vector_with(
        RcppRoll::min_f(), NumericVector(x), weights, n, pad, normalize);
  }

}

// [[Rcpp::export(.RcppRoll_max)]]
SEXP roll_max(SEXP x, int n, SEXP weights, SEXP pad, bool by_column, bool normalize) {

  if (Rf_isMatrix(x)) {
    return RcppRoll::roll_matrix_with(
        RcppRoll::max_f(), NumericMatrix(x), weights, n, pad, normalize);
  } else {
    return RcppRoll::roll_vector_with(
        RcppRoll::max_f(), NumericVector(x), weights, n, pad, normalize);
  }

}

// [[Rcpp::export(.RcppRoll_prod)]]
SEXP roll_prod(SEXP x, int n, SEXP weights, SEXP pad, bool by_column, bool normalize) {

  if (Rf_isMatrix(x)) {
    return RcppRoll::roll_matrix_with(
        RcppRoll::prod_f(), NumericMatrix(x), weights, n, pad, normalize);
  } else {
    return RcppRoll::roll_vector_with(
        RcppRoll::prod_f(), NumericVector(x), weights, n, pad, normalize);
  }

}

// [[Rcpp::export(.RcppRoll_sum)]]
SEXP roll_sum(SEXP x, int n, SEXP weights, SEXP pad, bool by_column, bool normalize) {

  if (Rf_isMatrix(x)) {
    return RcppRoll::roll_matrix_with(
        RcppRoll::sum_f(), NumericMatrix(x), weights, n, pad, normalize);
  } else {
    return RcppRoll::roll_vector_with(
        RcppRoll::sum_f(), NumericVector(x), weights, n, pad, normalize);
  }

}

// End auto-generated exports (internal/make_exports.R)
