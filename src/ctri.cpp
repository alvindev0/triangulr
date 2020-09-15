#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

ComplexVector CTri(NumericVector t, double min, double max, double mode) {
  int n = t.size();

  if (min >= max || mode > max || min > mode)
  {
    warning("\nNaN(s) produced."
              "\n* min must be less than max"
              "\n* min must be less than or equal to mode"
              "\n* mode must be less than or equal to max");
    return ComplexVector(n, complex<double>(R_NaN, 0.0));
  }

  complex<double> x(0.0, 1.0);
  ComplexVector c(n);
  Rcomplex rc;
  for (int i = 0; i < n; i++)
  {
    complex<double> cc = -2.0 * ((max - mode) * exp(x * min * t[i]) -
      (max - min) * exp(x * mode * t[i]) + (mode - min) * exp(x * max * t[i])) /
        ((max - min) * (mode - min) * (max - mode) * pow(t[i], 2));
    rc.r = cc.real();
    rc.i = cc.imag();
    c[i] = rc;
  }
  return c;
}

ComplexVector CTri(
    NumericVector t, NumericVector min, NumericVector max, NumericVector mode
) {
  int n = t.size();

  complex<double> x(0.0, 1.0);
  ComplexVector c(n);
  Rcomplex rc;
  for (int i = 0; i < n; i++)
  {
    if (min[i] >= max[i] || mode[i] > max[i] || min[i] > mode[i])
    {
      warning("\nNaN(s) produced."
                "\n* min must be less than max"
                "\n* min must be less than or equal to mode"
                "\n* mode must be less than or equal to max");
      rc.r = R_NaN;
      rc.i = 0.0;
      c[i] = rc;
    }
    else
    {
      complex<double> cc = -2.0 * ((max[i] - mode[i]) * exp(x * min[i] * t[i]) -
        (max[i] - min[i]) * exp(x * mode[i] * t[i]) + (mode[i] - min[i]) *
        exp(x * max[i] * t[i])) /
          ((max[i] - min[i]) * (mode[i] - min[i]) * (max[i] - mode[i]) *
            pow(t[i], 2));
      rc.r = cc.real();
      rc.i = cc.imag();
      c[i] = rc;
    }
  }
  return c;
}

// [[Rcpp::export]]
ComplexVector CTriC(NumericVector t, double min, double max, double mode) {
  return CTri(t, min, max, mode);
}

// [[Rcpp::export]]
ComplexVector CTriC2(
    NumericVector t, NumericVector min, NumericVector max, NumericVector mode
) {
  return CTri(t, min, max, mode);
}

/*** R

*/
