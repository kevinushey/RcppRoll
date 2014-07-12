#include <Rcpp.h>

using namespace Rcpp;
using namespace std;

const double flagval = __DBL_MIN__; // works
//const double flagval = NA_REAL;   // does not

// simple double value 'flagging' function
inline double flag(double a, bool b) { return b ? a : flagval; }

// [[Rcpp::export]]
NumericVector subsetter(NumericVector a, LogicalVector b) {
  // We use the flag() function to mark values of 'a' 
  // for which 'b' is false with the 'flagval'
  transform(a.begin(), a.end(), b.begin(), a.begin(), flag);
  
  // We use sugar's sum to compute how many true values to expect
    NumericVector res = NumericVector(sum(b));

    // And then copy the ones different from flagval from a into
    // res using the remove_copy function from the STL
    remove_copy(a.begin(), a.end(), res.begin(), flagval);
    return res;    
}