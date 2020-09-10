#include <Rcpp.h>
#include <random>

#include "qtriang.h"

using namespace Rcpp;

Rcpp::NumericVector RTriang(int n, double min, double max, double mode)
{
  return QTriang(Rcpp::runif(n), min, max, mode, true, false);
}

Rcpp::NumericVector RTriang(
    int n, Rcpp::NumericVector min, Rcpp::NumericVector max,
    Rcpp::NumericVector mode
) {
  return QTriang(Rcpp::runif(n), min, max, mode, true, false);
}

// [[Rcpp::export]]
Rcpp::NumericVector RTriangC(int n, double min, double max, double mode)
{
  return RTriang(n, min, max, mode);
}

// [[Rcpp::export]]
Rcpp::NumericVector RTriangC2(
    int n, Rcpp::NumericVector min, Rcpp::NumericVector max,
    Rcpp::NumericVector mode
) {
  return RTriang(n, min, max, mode);
}

/*** R

*/
