// [[Rcpp::export(.RcppRoll_%s)]]
SEXP roll_%s(SEXP x,
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
        RcppRoll::%s_f<true>(), NumericMatrix(x), n, weights, by, fill, partial, align, normalize);
    } else {
      return RcppRoll::roll_matrix_with(
        RcppRoll::%s_f<false>(), NumericMatrix(x), n, weights, by, fill, partial, align, normalize);
    }
  } else {
    if (na_rm) {
      return RcppRoll::roll_vector_with(
        RcppRoll::%s_f<true>(), NumericVector(x), n, weights, by, fill, partial, align, normalize);
    } else {
      return RcppRoll::roll_vector_with(
        RcppRoll::%s_f<false>(), NumericVector(x), n, weights, by, fill, partial, align, normalize);
    }
  }
}
