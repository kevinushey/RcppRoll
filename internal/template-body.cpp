extern "C" SEXP roll_%s_impl(SEXP x,
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
      RcppRoll::%s_f<true>(), x, Rf_asInteger(n), weights, Rf_asInteger(by),
      fill_, Rf_asLogical(partial), CHAR(STRING_ELT(align, 0)),
      Rf_asLogical(normalize));
  } else {
    return RcppRoll::roll_with(
      RcppRoll::%s_f<false>(), x, Rf_asInteger(n), weights, Rf_asInteger(by),
      fill_, Rf_asLogical(partial), CHAR(STRING_ELT(align, 0)),
      Rf_asLogical(normalize));
  }
}
