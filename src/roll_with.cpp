#include <Rcpp.h>
using namespace Rcpp;

namespace RcppRoll {

template <typename T, typename U = decltype(T()[0])>
class VectorView {

public:

  VectorView(T const& data, int offset):
    data_(data), offset_(offset) {};

  inline const U operator[](int idx) const {
    return const_cast<const U>(data_[offset_ + idx]);
  }

private:

  T const& data_;
  int offset_;

};

template <typename T>
std::vector<double> operator*(VectorView<T> const& lhs, NumericVector const& rhs) {
  std::vector<double> output;
  output.reserve(rhs.size());
  for (int i = 0; i < rhs.size(); ++i) {
    output.emplace_back(lhs[i] * rhs[i]);
  }
  return output;
}

template <typename T>
struct product {
  inline T operator()(T const& left, T const& right) {
    return left * right;
  }
};

template <typename T>
double prod(T const& x) {
  return std::accumulate(x.begin(), x.end(), 1.0, product<double>());
}

template <typename Callable, typename T>
T roll_vector_with(Callable f, T const& x, NumericVector weights, SEXP pad) {

  if (Rf_isNull(pad)) {
    return roll_vector_with_nopad(f, x, weights);
  } else if (strcmp(CHAR(STRING_ELT(pad, 0)), "left") == 0) {
    return roll_vector_with_pad_left(f, x, weights);
  } else if (strcmp(CHAR(STRING_ELT(pad, 0)), "right") == 0) {
    return roll_vector_with_pad_right(f, x, weights);
  } else {
    return roll_vector_with_nopad(f, x, weights);
  }

}

template <typename Callable, typename T>
T roll_vector_with_nopad(Callable f, T const& x, NumericVector weights) {

  int x_n = x.size();
  int weights_n = weights.size();
  int n_ops = x_n - weights_n + 1;

  T result = no_init(n_ops);
  for (int i = 0; i < n_ops; ++i) {
    result[i] = f(RcppRoll::VectorView<T>(x, i) * weights);
  }

  return result;

}

template <typename Callable, typename T>
T roll_vector_with_pad_left(Callable f, T const& x, NumericVector weights) {

  int x_n = x.size();
  int weights_n = weights.size();
  int n_ops = x_n - weights_n + 1;

  T result = no_init(x_n);
  for (int i = 0; i < weights_n - 1; ++i) {
    result[i] = T::get_na();
  }
  for (int i = 0; i < n_ops; ++i) {
    result[i + weights_n - 1] = f(VectorView<T>(x, i) * weights);
  }

  return result;

}

template <typename Callable, typename T>
T roll_vector_with_pad_right(Callable f, T const& x, NumericVector weights) {

  int x_n = x.size();
  int weights_n = weights.size();

  T result = no_init(x_n);
  for (int i = 0; i < x_n - weights_n + 1; ++i) {
    result[i] = f(VectorView<T>(x, i) * weights);
  }
  for (int i = x_n - weights_n + 1; i < x_n; ++i) {
    result[i] = T::get_na();
  }

  return result;

}

template <typename Callable, typename T>
T roll_matrix_with(Callable f, T const& x, NumericVector weights, SEXP pad) {

  int nrow = x.nrow();
  int ncol = x.ncol();

  T output(nrow - weights.size() + 1, ncol);
  for (int i = 0; i < ncol; ++i) {
    output(_, i) = roll_vector_with(f, static_cast<NumericVector>(x(_, i)), weights, pad);
  }

  return output;

}

struct mean_f {
  inline double operator()(std::vector<double> const& x) {
    return std::accumulate(x.begin(), x.end(), 0.0) / x.size();
  }
};

struct sum_f {
  inline double operator()(std::vector<double> const& x) {
    return std::accumulate(x.begin(), x.end(), 0.0);
  }
};

struct min_f {
  template <typename T>
  T operator()(std::vector<T> const& x) {
    return *std::min_element(x.begin(), x.end());
  }
};

struct max_f {
  template <typename T>
  T operator()(std::vector<T> const& x) {
    return *std::max_element(x.begin(), x.end());
  }
};

struct prod_f {
  template <typename T>
  T operator()(std::vector<T> const& x) {
    return std::accumulate(x.begin(), x.end(), 1.0, product<T>());
  }
};

std::vector<double> mean_normalize(std::vector<double> const& x) {
  double mu = mean_f()(x);
  std::vector<double> output;
  int n = x.size();
  output.reserve(n);
  for (int i = 0; i < n; ++i) {
    output.emplace_back(x[i] - mu);
  }
  return output;
}

struct var_f {
  inline double operator()(std::vector<double> const& x) {
    int n = x.size();
    std::vector<double> normalized;
    normalized.reserve(n);
    double mu = mean_f()(x);
    for (int i = 0; i < n; ++i) {
      normalized.push_back(x[i] - mu);
    }
    return sum_f()(normalized) / (n - 1);
  }
};

struct sd_f {
  inline double operator()(std::vector<double> const& x) {
    return sqrt(var_f()(x));
  }
};

std::vector<double> square(std::vector<double> const& x, int exponent) {
  std::vector<double> result;
  result.reserve(x.size());
  std::transform(x.begin(), x.begin(), result.begin(), [](double x) {
    return x * x;
  });
  return result;
}

struct median_f {
  inline double operator()(std::vector<double> x) {

    double median;

    size_t size = x.size();
    sort(x.begin(), x.end());

    if (size % 2 == 0) {
      median = (x[size / 2 - 1] + x[size / 2]) / 2;
    } else {
      median = x[size / 2];
    }

    return median;
  }
};

} // end namespace RcppRoll

using namespace RcppRoll;

#define DISPATCH(__FUNCTION__) \
  if (Rf_isMatrix(x)) { \
    return roll_matrix_with(__FUNCTION__##_f(), NumericMatrix(x), weights, pad); \
  } else { \
    return roll_vector_with(__FUNCTION__##_f(), NumericVector(x), weights, pad); \
  }

// [[Rcpp::export]]
SEXP roll_mean(SEXP x, SEXP weights, SEXP pad) {
  DISPATCH(mean);
}

// [[Rcpp::export]]
SEXP roll_median(SEXP x, SEXP weights, SEXP pad) {
  DISPATCH(median);
}

// [[Rcpp::export]]
SEXP roll_max(NumericVector x, NumericVector weights, SEXP pad) {
  DISPATCH(max);
}

// [[Rcpp::export]]
SEXP roll_min(NumericVector x, NumericVector weights, SEXP pad) {
  DISPATCH(min);
}

// [[Rcpp::export]]
SEXP roll_prod(NumericVector x, NumericVector weights, SEXP pad) {
  DISPATCH(prod);
}

// [[Rcpp::export]]
SEXP roll_sd(NumericVector x, NumericVector weights, SEXP pad) {
  DISPATCH(sd);
}

// [[Rcpp::export]]
SEXP roll_var(NumericVector x, NumericVector weights, SEXP pad) {
  DISPATCH(var);
}

/*** R
x <- c(1, 2, 3)
weights <- c(1, 1)
pad <- NULL
roll_mean(x, weights, pad)
pad <- "left"
roll_mean(x, weights, pad)
pad <- "right"
roll_mean(x, weights, pad)
x <- rnorm(1E4)
library(microbenchmark)
library(zoo)
microbenchmark(
  roll_mean(x, weights, NULL),
  rollapply(x, weights, mean),
  roll_max(x, weights, NULL),
  roll_min(x, weights, NULL),
  roll_prod(x, weights, NULL),
  roll_prod(x, weights, "left"),
  roll_prod(x, weights, "right"),
  times = 5
)
*/
