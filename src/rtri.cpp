#include <Rcpp.h>
#include <random>

#include "qtri.h"

using namespace Rcpp;

Rcpp::NumericVector RTri(int n, double min, double max, double mode)
{
  return QTri(Rcpp::runif(n), min, max, mode, true, false);
}

Rcpp::NumericVector RTri(
    int n, Rcpp::NumericVector min, Rcpp::NumericVector max,
    Rcpp::NumericVector mode
) {
  return QTri(Rcpp::runif(n), min, max, mode, true, false);
}

// [[Rcpp::export]]
Rcpp::NumericVector RTriC(int n, double min, double max, double mode)
{
  return RTri(n, min, max, mode);
}

// [[Rcpp::export]]
Rcpp::NumericVector RTriC2(
    int n, Rcpp::NumericVector min, Rcpp::NumericVector max,
    Rcpp::NumericVector mode
) {
  return RTri(n, min, max, mode);
}

/*** R

*/
