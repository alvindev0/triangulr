#include <Rcpp.h>
using namespace Rcpp;

NumericVector ESTri(
    NumericVector p, double min, double max, double mode, bool lower_tail,
    bool log_p
) {
  if (log_p)
  {
    p = exp(p);
  }

  if (!lower_tail)
  {
    p = 1.0 - p;
  }

  int n = p.size();

  if (min >= max || mode > max || min > mode)
  {
    warning("\nNaN(s) produced."
              "\n* min must be less than max"
              "\n* min must be less than or equal to mode"
              "\n* mode must be less than or equal to max");
    return NumericVector(n, R_NaN);
  }

  NumericVector es(n);
  for (int i = 0; i < n; i++)
  {
    if (p[i] == 0.0 || p[i] < 0.0 || p[i] > 1.0)
    {
      warning("\nNaN produced."
                "\n* p must be greater than 0 and less than or equal to 1");
      es[i] = R_NaN;
    }
    else if (p[i] < (mode - min) / (max - min))
    {
      es[i] = (
        (p[i] * min) + (2.0 / 3.0) * sqrt((max - min) * (mode - min)) *
          pow(p[i], 1.5)
      ) / p[i];
    }
    else // if (p[i] >= (mode - min) / (max - min))
    {
      double b = (mode - min) / (max - min);
      es[i] = (
        (b * min) + (2.0 / 3.0) * sqrt((max - min) * (mode - min)) *
          pow(b, 1.5) +
          (
            ((p[i] * max) + (2.0 / 3.0) * sqrt((max - min) * (max - mode)) *
              pow((1.0 - p[i]), 1.5)) -
              ((b * max) + (2.0 / 3.0) * sqrt((max - min) * (max - mode)) *
              pow((1.0 - b), 1.5))
          )
      ) / p[i];
    }
  }
  return es;
}

NumericVector ESTri(
    NumericVector p, NumericVector min, NumericVector max, NumericVector mode,
    bool lower_tail, bool log_p
) {
  if (log_p)
  {
    p = exp(p);
  }

  if (!lower_tail)
  {
    p = 1.0 - p;
  }

  int n = p.size();

  NumericVector es(n);
  for (int i = 0; i < n; i++)
  {
    if (min[i] >= max[i] || mode[i] > max[i] || min[i] > mode[i])
    {
      warning("\nNaN produced."
                "\n* min must be less than max"
                "\n* min must be less than or equal to mode"
                "\n* mode must be less than or equal to max");
      es[i] = R_NaN;
    }
    else if (p[i] == 0.0 || p[i] < 0.0 || p[i] > 1.0)
    {
      warning("\nNaN produced."
                "\n* p must be greater than 0 and less than or equal to 1");
      es[i] = R_NaN;
    }
    else if (p[i] < (mode[i] - min[i]) / (max[i] - min[i]))
    {
      es[i] = (
        (p[i] * min[i]) + (2.0 / 3.0) * sqrt((max[i] - min[i]) *
          (mode[i] - min[i])) * pow(p[i], 1.5)
      ) / p[i];
    }
    else // if (p[i] >= (mode - min) / (max - min))
    {
      double b = (mode[i] - min[i]) / (max[i] - min[i]);
      es[i] = (
        (b * min[i]) + (2.0 / 3.0) * sqrt((max[i] - min[i]) *
          (mode[i] - min[i])) * pow(b, 1.5) +
          (
              ((p[i] * max[i]) + (2.0 / 3.0) * sqrt((max[i] - min[i]) *
                (max[i] - mode[i])) * pow((1.0 - p[i]), 1.5)) -
                ((b * max[i]) + (2.0 / 3.0) * sqrt((max[i] - min[i]) *
                (max[i] - mode[i])) * pow((1.0 - b), 1.5))
          )
      ) / p[i];
    }
  }
  return es;
}

// [[Rcpp::export]]
NumericVector ESTriC(
    NumericVector p, double min, double max, double mode, bool lower_tail,
    bool log_p
) {
  return ESTri(p, min, max, mode, lower_tail, log_p);
}

// [[Rcpp::export]]
NumericVector ESTriC2(
    NumericVector p, NumericVector min, NumericVector max, NumericVector mode,
    bool lower_tail, bool log_p
) {
  return ESTri(p, min, max, mode, lower_tail, log_p);
}

/*** R

*/
