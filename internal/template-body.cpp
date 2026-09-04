// [[Rcpp::export]]
SEXP roll_%s_impl(SEXP x,
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
      RcppRoll::%s_f<true>(), x, n, weights, by, fill, partial, align, normalize);
  } else {
    return RcppRoll::roll_with(
      RcppRoll::%s_f<false>(), x, n, weights, by, fill, partial, align, normalize);
  }
}
