functions <- c("mean", "median", "min", "max", "prod", "sum", "sd", "var")
preamble <-
"#include <Rcpp.h>
using namespace Rcpp;
"
generator <-
"
struct %s_f;

// [[Rcpp::export]]
SEXP roll_%s(SEXP x, NumericVector weights, SEXP pad, bool by_column, bool normalize) {
  if (normalize)
    weights = weights / sum(weights);
  if (Rf_isMatrix(x)) { \
    return roll_matrix_with(%s_f(), NumericMatrix(x), weights, pad); \
  } else { \
    return roll_vector_with(%s_f(), NumericVector(x), weights, pad); \
  }
}
"

gen <- unlist(lapply(functions, function(x) {
  gsub("%s", x, generator, fixed = TRUE)
}))

cat(c(preamble, gen), sep = "\n", file = "src/exports_gen.cpp")
