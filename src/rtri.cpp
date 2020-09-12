#include <Rcpp.h>
#include <random>

#include "qtri.h"

using namespace Rcpp;

NumericVector RTri(int n, double min, double max, double mode)
{
  return QTri(runif(n), min, max, mode, true, false);
}

NumericVector RTri(
    int n, NumericVector min, NumericVector max, NumericVector mode
) {
  return QTri(runif(n), min, max, mode, true, false);
}

// [[Rcpp::export]]
NumericVector RTriC(int n, double min, double max, double mode)
{
  return RTri(n, min, max, mode);
}

// [[Rcpp::export]]
NumericVector RTriC2(
    int n, NumericVector min, NumericVector max, NumericVector mode
) {
  return RTri(n, min, max, mode);
}

/*** R

*/
