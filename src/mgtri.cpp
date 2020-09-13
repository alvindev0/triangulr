#include <Rcpp.h>
using namespace Rcpp;

NumericVector MGTri(
    NumericVector t, double min, double max, double mode
) {
  int n = t.size();
  NumericVector m(n);
  for (int i = 0; i < n; i++)
  {
    m[i] = 2 * ((max - mode) * exp(min * t[i]) - (max - min) *
      exp(mode * t[i]) + (mode - min) * exp(max * t[i])) /
        ((max - min) * (mode - min) * (max - mode) *
          pow(t[i], 2));
  }
  return m;
}

NumericVector MGTri(
    NumericVector t, NumericVector min, NumericVector max, NumericVector mode
) {
  int n = t.size();
  NumericVector m(n);
  for (int i = 0; i < n; i++)
  {
    m[i] = 2 * ((max[i] - mode[i]) * exp(min[i] * t[i]) - (max[i] - min[i]) *
      exp(mode[i] * t[i]) + (mode[i] - min[i]) * exp(max[i] * t[i])) /
        ((max[i] - min[i]) * (mode[i] - min[i]) * (max[i] - mode[i]) *
          pow(t[i], 2));
  }
  return m;
}

// [[Rcpp::export]]
NumericVector MGTriC(NumericVector t, double min, double max, double mode) {
  return MGTri(t, min, max, mode);
}

// [[Rcpp::export]]
NumericVector MGTriC2(
    NumericVector t, NumericVector min, NumericVector max, NumericVector mode
) {
  return MGTri(t, min, max, mode);
}

/*** R

*/
