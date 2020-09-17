#include <Rcpp.h>
#include "qtri.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector QTriC(
    NumericVector p, double min, double max, double mode, bool lower_tail,
    bool log_p
) {
  int n = p.size();

  if (min >= max || mode > max || min > mode)
  {
    warning("\nNaN(s) produced."
              "\n* min must be less than max"
              "\n* min must be less than or equal to mode"
              "\n* mode must be less than or equal to max.");
    return NumericVector(n, R_NaN);
  }

  NumericVector q = Rcpp::clone(p);

  if (log_p)
  {
    q = exp(q);
  }

  if (!lower_tail)
  {
    q = 1.0 - q;
  }

  double int_len = max - min;
  for (int i = 0; i < n; i++)
  {
    if (q[i] < 0.0 || q[i] > 1.0)
    {
      warning("\nNaN produced."
                "\n* p must be between 0 and 1 inclusive");
      q[i] = R_NaN;
    }
    else if (q[i] < (mode - min) / int_len)
    {
      q[i] = min + sqrt(q[i] * int_len * (mode - min));
    }
    else // if (q[i] >= (mode - min) / int_len)
    {
      q[i] = max - sqrt((1.0 - q[i]) * int_len * (max - mode));
    }
  }

  return q;
}

// [[Rcpp::export]]
NumericVector QTriC2(
    NumericVector p, NumericVector min, NumericVector max, NumericVector mode,
    bool lower_tail, bool log_p
) {
  int n = p.size();

  NumericVector q = Rcpp::clone(p);
  
  if (log_p)
  {
    q = exp(q);
  }

  if (!lower_tail)
  {
    q = 1.0 - q;
  }

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
    else if (q[i] < 0.0 || q[i] > 1.0)
    {
      warning("\nNaN produced."
                "\n* p must be between 0 and 1 inclusive.");
      q[i] = R_NaN;
    }
    else if (q[i] < (mode[i] - min[i]) / (max[i] - min[i]))
    {
      q[i] = min[i] + sqrt(q[i] * (max[i] - min[i]) * (mode[i] - min[i]));
    }
    else  // if (q[i] >= (mode[i] - min[i]) / (max[i] - min[i]))
    {
      q[i] = max[i] - sqrt((1.0 - q[i]) * (max[i] - min[i]) * (max[i] - mode[i]));
    }
  }

  return q;
}

/*** R

*/
