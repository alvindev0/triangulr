#include <Rcpp.h>
#include "qtri.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector RTriC(int n, double min, double max, double mode)
{
  return QTriC(runif(n), min, max, mode, true, false);
}

// [[Rcpp::export]]
NumericVector RTriC2(
    int n, NumericVector min, NumericVector max, NumericVector mode
) {
  return QTriC2(runif(n), min, max, mode, true, false);
}

/*** R

*/
