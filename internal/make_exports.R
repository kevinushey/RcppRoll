functions <- c("mean", "median", "min", "max", "prod", "sum", "sd", "var")
preamble <-
  "#include <Rcpp.h>
using namespace Rcpp;
"
generator <-
  "
// [[Rcpp::export]]
SEXP roll_%s(SEXP x, NumericVector weights, SEXP pad, bool by_column, bool normalize) {
  if (normalize)
    weights = weights / sum(weights);
  if (Rf_isMatrix(x)) { \
    return RcppRoll::roll_matrix_with(RcppRoll::%s_f(), NumericMatrix(x), weights, pad); \
  } else { \
    return RcppRoll::roll_vector_with(RcppRoll::%s_f(), NumericVector(x), weights, pad); \
  }
}
"

gen <- unlist(lapply(functions, function(x) {
  gsub("%s", x, generator, fixed = TRUE)
}))

has_flag <- function(content, flag) {
  any(grepl(flag, content, fixed = TRUE))
}

content <- readLines("src/roll_with.cpp")
begin_flag <- "// Begin auto-generated exports (internal/make_exports.R)"
end_flag <- "// End auto-generated exports (internal/make_exports.R)"
if (has_flag(content, begin_flag) && has_flag(content, end_flag)) {
  content <- content[-c(`:`(
    grep(begin_flag, content, fixed = TRUE),
    grep(end_flag, content, fixed = TRUE)
  ))]
}

output <- c(content,
            begin_flag,
            gen,
            end_flag
)

cat(output, file = "src/roll_with.cpp", sep = "\n")
