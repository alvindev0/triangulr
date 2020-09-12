#include <Rcpp.h>
using namespace Rcpp;

Rcpp::NumericVector DTri(
    Rcpp::NumericVector x, double min, double max, double mode, bool log
) {
  int n = x.size();

  if (min >= max || mode > max || min > mode)
  {
    Rcpp::warning("\nNaN(s) produced."
                    "\n* min must be less than max"
                    "\n* min must be less than or equal to mode"
                    "\n* mode must be less than or equal to max.");
    return Rcpp::NumericVector(n, R_NaN);
  }

  Rcpp::NumericVector d(n);
  double int_len = max - min;
  for (int i = 0; i < n; i++)
  {
    if (x[i] < min || x[i] > max)
    {
      d[i] = 0;
    }
    else if (min <= x[i] && x[i] <= mode)
    {
      d[i] = 2 * (x[i] - min) / (int_len * (mode - min));
    }
    else if (mode < x[i] && x[i] <= max)
    {
      d[i] = 2 * (max - x[i]) / (int_len * (max - mode));
    }
  }

  if (log)
  {
    d = Rcpp::log(d);
  }

  return d;
}

Rcpp::NumericVector DTri(
    Rcpp::NumericVector x, Rcpp::NumericVector min, Rcpp::NumericVector max,
    Rcpp::NumericVector mode, bool log
) {
  int n = x.size();

  Rcpp::NumericVector d(n);
  for (int i = 0; i < n; i++)
  {
    if (min[i] >= max[i] || mode[i] > max[i] || min[i] > mode[i])
    {
      Rcpp::warning("\nNaN produced."
                      "\n* min must be less than max"
                      "\n* min must be less than or equal to mode"
                      "\n* mode must be less than or equal to max");
      d[i] = R_NaN;
    }
    else if (x[i] < min[i] || x[i] > max[i])
    {
      d[i] = 0;
    }
    else if (min[i] <= x[i] && x[i] <= mode[i])
    {
      d[i] = 2 * (x[i] - min[i]) / ((max[i] - min[i]) * (mode[i] - min[i]));
    }
    else if (mode[i] < x[i] && x[i] <= max[i])
    {
      d[i] = 2 * (max[i] - x[i]) / ((max[i] - min[i]) * (max[i] - mode[i]));
    }
  }

  if (log)
  {
    d = Rcpp::log(d);
  }

  return d;
}

// [[Rcpp::export]]
Rcpp::NumericVector DTriC(
    Rcpp::NumericVector x, double min, double max, double mode, bool log
) {
  return DTri(x, min, max, mode, log);
}

// [[Rcpp::export]]
Rcpp::NumericVector DTriC2(
    Rcpp::NumericVector x, Rcpp::NumericVector min, Rcpp::NumericVector max,
    Rcpp::NumericVector mode, bool log
) {
  return DTri(x, min, max, mode, log);
}

/*** R

*/
