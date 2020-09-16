#include <Rcpp.h>
using namespace Rcpp;

NumericVector DTri(
    NumericVector x, double min, double max, double mode, bool log
) {
  int n = x.size();

  if (min >= max || mode > max || min > mode)
  {
    warning("\nNaN(s) produced."
              "\n* min must be less than max"
              "\n* min must be less than or equal to mode"
              "\n* mode must be less than or equal to max.");
    return NumericVector(n, R_NaN);
  }

  NumericVector d(n);
  for (int i = 0; i < n; i++)
  {
    if (x[i] < min || x[i] > max)
    {
      d[i] = 0.0;
    }
    else if (min <= x[i] && x[i] < mode)
    {
      d[i] = 2.0 * (x[i] - min) / ((max - min) * (mode - min));
    }
    else if (x[i] == mode)
    {
      d[i] = 2.0 / (max - min);
    }
    else // if (mode < x[i] && x[i] <= max)
    {
      d[i] = 2.0 * (max - x[i]) / ((max - min) * (max - mode));
    }
  }

  if (log)
  {
    return Rcpp::log(d);
  }

  return d;
}

NumericVector DTri(
    NumericVector x, NumericVector min, NumericVector max, NumericVector mode,
    bool log
) {
  int n = x.size();

  NumericVector d(n);
  for (int i = 0; i < n; i++)
  {
    if (min[i] >= max[i] || mode[i] > max[i] || min[i] > mode[i])
    {
      warning("\nNaN produced."
                "\n* min must be less than max"
                "\n* min must be less than or equal to mode"
                "\n* mode must be less than or equal to max");
      d[i] = R_NaN;
    }
    else if (x[i] < min[i] || x[i] > max[i])
    {
      d[i] = 0.0;
    }
    else if (min[i] <= x[i] && x[i] < mode[i])
    {
      d[i] = 2.0 * (x[i] - min[i]) / ((max[i] - min[i]) * (mode[i] - min[i]));
    }
    else if (x[i] == mode[i])
    {
      d[i] = 2.0 / (max[i] - min[i]);
    }
    else // if (mode[i] < x[i] && x[i] <= max[i])
    {
      d[i] = 2.0 * (max[i] - x[i]) / ((max[i] - min[i]) * (max[i] - mode[i]));
    }
  }

  if (log)
  {
    return Rcpp::log(d);
  }

  return d;
}

// [[Rcpp::export]]
NumericVector DTriC(
    NumericVector x, double min, double max, double mode, bool log
) {
  return DTri(x, min, max, mode, log);
}

// [[Rcpp::export]]
NumericVector DTriC2(
    NumericVector x, NumericVector min, NumericVector max, NumericVector mode,
    bool log
) {
  return DTri(x, min, max, mode, log);
}

/*** R

*/
