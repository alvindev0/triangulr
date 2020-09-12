#include <Rcpp.h>

#include "qtri.h"

using namespace Rcpp;

NumericVector QTri(
    NumericVector p, double min, double max, double mode, bool lower_tail,
    bool log_p
) {
  if (log_p)
  {
    p = exp(p);
  }

  if (!lower_tail)
  {
    p = 1 - p;
  }

  int n = p.size();

  if (min >= max || mode > max || min > mode)
  {
    warning("\nNaN(s) produced."
              "\n* min must be less than max"
              "\n* min must be less than or equal to mode"
              "\n* mode must be less than or equal to max.");
    return NumericVector(n, R_NaN);
  }

  NumericVector q(n);
  double int_len = max - min;
  for (int i = 0; i < n; i++)
  {
    if (p[i] < 0 || p[i] > 1)
    {
      warning("\nNaN produced."
                "\n* p must be between 0 and 1 inclusive");
      q[i] = R_NaN;
    }
    else if (p[i] < (mode - min) / int_len)
    {
      q[i] = min + sqrt(p[i] * int_len * (mode - min));
    }
    else
    {
      q[i] = max - sqrt((1.0 - p[i]) * int_len * (max - mode));
    }
  }

  return q;
}

NumericVector QTri(
    NumericVector p, NumericVector min, NumericVector max, NumericVector mode,
    bool lower_tail, bool log_p
) {
  if (log_p)
  {
    p = exp(p);
  }

  if (!lower_tail)
  {
    p = 1 - p;
  }

  int n = p.size();

  NumericVector q(n);
  for (int i = 0; i < n; i++)
  {
    if (min[i] >= max[i] || mode[i] > max[i] || min[i] > mode[i])
    {
      warning("\nNaN produced."
                "\n* min must be less than max"
                "\n* min must be less than or equal to mode"
                "\n* mode must be less than or equal to max");
      q[i] = R_NaN;
    }
    else if (p[i] < 0 || p[i] > 1)
    {
      warning("\nNaN produced."
                "\n* p must be between 0 and 1 inclusive.");
      q[i] = R_NaN;
    }
    else if (p[i] < (mode[i] - min[i]) / (max[i] - min[i]))
    {
      q[i] = min[i] + sqrt(p[i] * (max[i] - min[i]) * (mode[i] - min[i]));
    }
    else
    {
      q[i] = max[i] - sqrt((1 - p[i]) * (max[i] - min[i]) * (max[i] - mode[i]));
    }
  }

  return q;
}

// [[Rcpp::export]]
NumericVector QTriC(
    NumericVector p, double min, double max, double mode, bool lower_tail,
    bool log_p
) {
  return QTri(p, min, max, mode, lower_tail, log_p);
}

// [[Rcpp::export]]
NumericVector QTriC2(
    NumericVector p, NumericVector min, NumericVector max, NumericVector mode,
    bool lower_tail, bool log_p
) {
  return QTri(p, min, max, mode, lower_tail, log_p);
}

/*** R

*/
