#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector ESTri(NumericVector p, double min, double max, double mode)
{
  int n = p.size();
  NumericVector es(n);
  for (int i = 0; i < n; i++)
  {
    if (p[i] == 0 || p[i] < 0 || p[i] > 1)
    {
      warning("\nNaN produced."
                "\n* p must be greater than 0 and less than or equal to 1");
      es[i] = R_NaN;
    }
    else if (p[i] < (mode - min) / (max - min))
    {
      es[i] = (
        (p[i] * min) + (2.0 / 3.0) * sqrt((max - min) * (mode-min)) *
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

/*** R

*/
