#include <Rcpp.h>
#include <random>

#include "qtriang.h"

using namespace Rcpp;

Rcpp::NumericVector RTriang(int n, double min, double max, double mode) {
  Rcpp::NumericVector p = Rcpp::NumericVector(Rcpp::runif(n));
  return QTriang(p, min, max, mode, true, false);
}

Rcpp::NumericVector RTriang(
    int n, Rcpp::NumericVector min, Rcpp::NumericVector max,
    Rcpp::NumericVector mode
) {
  Rcpp::NumericVector p = Rcpp::NumericVector(Rcpp::runif(n));
  return QTriang(p, min, max, mode, true, false);
}

// [[Rcpp::export]]
Rcpp::NumericVector RTriangC(int n, double min, double max, double mode) {
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
# C_rtriang(10, 0, 1, 0.5)
*/
