#include <Rcpp.h>
using namespace Rcpp;


struct mean_f;

// [[Rcpp::export]]
SEXP roll_mean(SEXP x, NumericVector weights, SEXP pad, bool by_column, bool normalize) {
  if (normalize)
    weights = weights / sum(weights);
  if (Rf_isMatrix(x)) { 
    return roll_matrix_with(mean_f(), NumericMatrix(x), weights, pad); 
  } else { 
    return roll_vector_with(mean_f(), NumericVector(x), weights, pad); 
  }
}


struct median_f;

// [[Rcpp::export]]
SEXP roll_median(SEXP x, NumericVector weights, SEXP pad, bool by_column, bool normalize) {
  if (normalize)
    weights = weights / sum(weights);
  if (Rf_isMatrix(x)) { 
    return roll_matrix_with(median_f(), NumericMatrix(x), weights, pad); 
  } else { 
    return roll_vector_with(median_f(), NumericVector(x), weights, pad); 
  }
}


struct min_f;

// [[Rcpp::export]]
SEXP roll_min(SEXP x, NumericVector weights, SEXP pad, bool by_column, bool normalize) {
  if (normalize)
    weights = weights / sum(weights);
  if (Rf_isMatrix(x)) { 
    return roll_matrix_with(min_f(), NumericMatrix(x), weights, pad); 
  } else { 
    return roll_vector_with(min_f(), NumericVector(x), weights, pad); 
  }
}


struct max_f;

// [[Rcpp::export]]
SEXP roll_max(SEXP x, NumericVector weights, SEXP pad, bool by_column, bool normalize) {
  if (normalize)
    weights = weights / sum(weights);
  if (Rf_isMatrix(x)) { 
    return roll_matrix_with(max_f(), NumericMatrix(x), weights, pad); 
  } else { 
    return roll_vector_with(max_f(), NumericVector(x), weights, pad); 
  }
}


struct prod_f;

// [[Rcpp::export]]
SEXP roll_prod(SEXP x, NumericVector weights, SEXP pad, bool by_column, bool normalize) {
  if (normalize)
    weights = weights / sum(weights);
  if (Rf_isMatrix(x)) { 
    return roll_matrix_with(prod_f(), NumericMatrix(x), weights, pad); 
  } else { 
    return roll_vector_with(prod_f(), NumericVector(x), weights, pad); 
  }
}


struct sum_f;

// [[Rcpp::export]]
SEXP roll_sum(SEXP x, NumericVector weights, SEXP pad, bool by_column, bool normalize) {
  if (normalize)
    weights = weights / sum(weights);
  if (Rf_isMatrix(x)) { 
    return roll_matrix_with(sum_f(), NumericMatrix(x), weights, pad); 
  } else { 
    return roll_vector_with(sum_f(), NumericVector(x), weights, pad); 
  }
}


struct sd_f;

// [[Rcpp::export]]
SEXP roll_sd(SEXP x, NumericVector weights, SEXP pad, bool by_column, bool normalize) {
  if (normalize)
    weights = weights / sum(weights);
  if (Rf_isMatrix(x)) { 
    return roll_matrix_with(sd_f(), NumericMatrix(x), weights, pad); 
  } else { 
    return roll_vector_with(sd_f(), NumericVector(x), weights, pad); 
  }
}


struct var_f;

// [[Rcpp::export]]
SEXP roll_var(SEXP x, NumericVector weights, SEXP pad, bool by_column, bool normalize) {
  if (normalize)
    weights = weights / sum(weights);
  if (Rf_isMatrix(x)) { 
    return roll_matrix_with(var_f(), NumericMatrix(x), weights, pad); 
  } else { 
    return roll_vector_with(var_f(), NumericVector(x), weights, pad); 
  }
}

