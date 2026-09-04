#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* defined in RcppRoll.cpp, with C linkage */
extern SEXP roll_mean_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP roll_median_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP roll_min_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP roll_max_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP roll_prod_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP roll_sum_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP roll_sd_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP roll_var_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP na_locf(SEXP);

static const R_CallMethodDef callEntries[] = {
  { "roll_mean_impl",   (DL_FUNC) &roll_mean_impl,   9 },
  { "roll_median_impl", (DL_FUNC) &roll_median_impl, 9 },
  { "roll_min_impl",    (DL_FUNC) &roll_min_impl,    9 },
  { "roll_max_impl",    (DL_FUNC) &roll_max_impl,    9 },
  { "roll_prod_impl",   (DL_FUNC) &roll_prod_impl,   9 },
  { "roll_sum_impl",    (DL_FUNC) &roll_sum_impl,    9 },
  { "roll_sd_impl",     (DL_FUNC) &roll_sd_impl,     9 },
  { "roll_var_impl",    (DL_FUNC) &roll_var_impl,    9 },
  { "na_locf",          (DL_FUNC) &na_locf,          1 },
  { NULL, NULL, 0 }
};

void R_init_RcppRoll(DllInfo* info) {
  R_registerRoutines(info, NULL, callEntries, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
}
