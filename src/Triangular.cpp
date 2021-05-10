#include <Rcpp.h>

using Rcpp::clone;
using Rcpp::ComplexVector;
using Rcpp::NumericVector;
using Rcpp::runif;
using Rcpp::warning;
using std::complex;

// [[Rcpp::export]]
NumericVector DTriC(
    NumericVector x, double min, double max, double mode, bool log)
{
  int n = x.size();

  if (min >= max || mode > max || min > mode)
  {
    warning("NaN(s) produced.");
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

// [[Rcpp::export]]
NumericVector DTriC2(
    NumericVector x, NumericVector min, NumericVector max, NumericVector mode,
    bool log)
{
  int n = x.size();
  bool has_nan = false;
  NumericVector d(n);

  for (int i = 0; i < n; i++)
  {
    if (min[i] >= max[i] || mode[i] > max[i] || min[i] > mode[i])
    {
      d[i] = R_NaN;
      has_nan = true;
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

  if (has_nan)
  {
    warning("NaN(s) produced.");
  }

  return d;
}

// [[Rcpp::export]]
NumericVector PTriC(
    NumericVector q, double min, double max, double mode, bool lower_tail,
    bool log_p)
{
  int n = q.size();

  if (min >= max || mode > max || min > mode)
  {
    warning("NaN(s) produced.");
    return NumericVector(n, R_NaN);
  }

  NumericVector p(n);
  for (int i = 0; i < n; i++)
  {
    if (q[i] <= min)
    {
      p[i] = 0.0;
    }
    else if (min < q[i] && q[i] <= mode)
    {
      p[i] = pow((q[i] - min), 2) / ((max - min) * (mode - min));
    }
    else if (mode < q[i] && q[i] < max)
    {
      p[i] = 1.0 - pow((max - q[i]), 2) / ((max - min) * (max - mode));
    }
    else // if (max <= q[i])
    {
      p[i] = 1.0;
    }
  }

  if (!lower_tail)
  {
    p = 1.0 - p;
  }

  if (log_p)
  {
    p = Rcpp::log(p);
  }

  return p;
}

// [[Rcpp::export]]
NumericVector PTriC2(
    NumericVector q, NumericVector min, NumericVector max, NumericVector mode,
    bool lower_tail, bool log_p)
{
  int n = q.size();
  bool has_nan = false;
  NumericVector p(n);

  for (int i = 0; i < n; i++)
  {
    if (min[i] >= max[i] || mode[i] > max[i] || min[i] > mode[i])
    {
      p[i] = R_NaN;
      has_nan = true;
    }
    else if (q[i] <= min[i])
    {
      p[i] = 0.0;
    }
    else if (min[i] < q[i] && q[i] <= mode[i])
    {
      p[i] = pow((q[i] - min[i]), 2) / ((max[i] - min[i]) * (mode[i] - min[i]));
    }
    else if (mode[i] < q[i] && q[i] < max[i])
    {
      p[i] = 1.0 - pow((max[i] - q[i]), 2) /
        ((max[i] - min[i]) * (max[i] - mode[i]));
    }
    else // if (max[i] <= q[i])
    {
      p[i] = 1.0;
    }
  }

  if (!lower_tail)
  {
    p = 1.0 - p;
  }

  if (log_p)
  {
    p = Rcpp::log(p);
  }

  if (has_nan)
  {
    warning("NaN(s) produced.");
  }

  return p;
}

// [[Rcpp::export]]
NumericVector QTriC(
    NumericVector p, double min, double max, double mode, bool lower_tail,
    bool log_p)
{
  int n = p.size();

  if (min >= max || mode > max || min > mode)
  {
    warning("NaN(s) produced.");
    return NumericVector(n, R_NaN);
  }

  NumericVector q = clone(p);

  if (log_p)
  {
    q = exp(q);
  }

  if (!lower_tail)
  {
    q = 1.0 - q;
  }

  bool has_nan = false;
  double int_len = max - min;

  for (int i = 0; i < n; i++)
  {
    if (q[i] < 0.0 || q[i] > 1.0)
    {
      q[i] = R_NaN;
      has_nan = true;
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

  if (has_nan)
  {
    warning("NaN(s) produced.");
  }

  return q;
}

// [[Rcpp::export]]
NumericVector QTriC2(
    NumericVector p, NumericVector min, NumericVector max, NumericVector mode,
    bool lower_tail, bool log_p)
{
  int n = p.size();
  NumericVector q = clone(p);

  if (log_p)
  {
    q = exp(q);
  }

  if (!lower_tail)
  {
    q = 1.0 - q;
  }

  bool has_nan = false;

  for (int i = 0; i < n; i++)
  {
    if (min[i] >= max[i] || mode[i] > max[i] || min[i] > mode[i] ||
        q[i] < 0.0 || q[i] > 1.0)
    {
      q[i] = R_NaN;
      has_nan = true;
    }
    else if (q[i] < (mode[i] - min[i]) / (max[i] - min[i]))
    {
      q[i] = min[i] + sqrt(q[i] * (max[i] - min[i]) * (mode[i] - min[i]));
    }
    else // if (q[i] >= (mode[i] - min[i]) / (max[i] - min[i]))
    {
      q[i] = max[i] - sqrt((1.0 - q[i]) * (max[i] - min[i]) * (max[i] - mode[i]));
    }
  }

  if (has_nan)
  {
    warning("NaN(s) produced.");
  }

  return q;
}

// [[Rcpp::export]]
NumericVector RTriC(int n, double min, double max, double mode)
{
  if (min >= max || mode > max || min > mode)
  {
    warning("NaN(s) produced.");
    return NumericVector(n, R_NaN);
  }

  NumericVector r = runif(n);
  double int_len = max - min;

  for (int i = 0; i < n; i++)
  {
    if (r[i] < (mode - min) / int_len)
    {
      r[i] = min + sqrt(r[i] * int_len * (mode - min));
    }
    else // if (r[i] >= (mode - min) / int_len)
    {
      r[i] = max - sqrt((1.0 - r[i]) * int_len * (max - mode));
    }
  }

  return r;
}

// [[Rcpp::export]]
NumericVector RTriC2(
    int n, NumericVector min, NumericVector max, NumericVector mode)
{
  NumericVector r = runif(n);
  bool has_nan = false;

  for (int i = 0; i < n; i++)
  {
    if (min[i] >= max[i] || mode[i] > max[i] || min[i] > mode[i])
    {
      r[i] = R_NaN;
      has_nan = true;
    }
    else if (r[i] < (mode[i] - min[i]) / (max[i] - min[i]))
    {
      r[i] = min[i] + sqrt(r[i] * (max[i] - min[i]) * (mode[i] - min[i]));
    }
    else // if (r[i] >= (mode[i] - min[i]) / (max[i] - min[i]))
    {
      r[i] = max[i] - sqrt((1.0 - r[i]) * (max[i] - min[i]) *
        (max[i] - mode[i]));
    }
  }

  if (has_nan)
  {
    warning("NaN(s) produced.");
  }

  return r;
}

// [[Rcpp::export]]
NumericVector MGTriC(
    NumericVector t, double min, double max, double mode)
{
  int n = t.size();

  if (min >= max || mode >= max || min >= mode)
  {
    warning("NaN(s) produced.");
    return NumericVector(n, R_NaN);
  }

  bool has_nan = false;
  NumericVector m(n);

  for (int i = 0; i < n; i++)
  {
    if (t[i] == 0.0)
    {
      m[i] = R_NaN;
      has_nan = true;
    }
    else
    {
      m[i] = 2.0 * ((max - mode) * exp(min * t[i]) - (max - min) * exp(mode * t[i]) + (mode - min) * exp(max * t[i])) /
        ((max - min) * (mode - min) * (max - mode) *
          pow(t[i], 2));
    }
  }

  if (has_nan)
  {
    warning("NaN(s) produced.");
  }

  return m;
}

// [[Rcpp::export]]
NumericVector MGTriC2(
    NumericVector t, NumericVector min, NumericVector max, NumericVector mode)
{
  int n = t.size();
  bool has_nan = false;
  NumericVector m(n);

  for (int i = 0; i < n; i++)
  {
    if (t[i] == 0.0 ||
        min[i] >= max[i] || mode[i] >= max[i] || min[i] >= mode[i])
    {
      m[i] = R_NaN;
      has_nan = true;
    }
    else
    {
      m[i] = 2.0 * ((max[i] - mode[i]) * exp(min[i] * t[i]) - (max[i] - min[i]) * exp(mode[i] * t[i]) + (mode[i] - min[i]) * exp(max[i] * t[i])) /
        ((max[i] - min[i]) * (mode[i] - min[i]) * (max[i] - mode[i]) *
          pow(t[i], 2));
    }
  }

  if (has_nan)
  {
    warning("NaN(s) produced.");
  }

  return m;
}

// [[Rcpp::export]]
ComplexVector CTriC(NumericVector t, double min, double max, double mode)
{
  int n = t.size();

  if (min >= max || mode >= max || min >= mode)
  {
    warning("NaN(s) produced.");
    return ComplexVector(n, complex<double>(R_NaN, 0.0));
  }

  bool has_nan = false;
  complex<double> x(0.0, 1.0);
  ComplexVector c(n);
  Rcomplex rc;

  for (int i = 0; i < n; i++)
  {
    if (t[i] == 0.0)
    {
      rc.r = R_NaN;
      rc.i = 0.0;
      c[i] = rc;
      has_nan = true;
    }
    else
    {
      complex<double> cc = -2.0 * ((max - mode) * exp(x * min * t[i]) - (max - min) * exp(x * mode * t[i]) + (mode - min) * exp(x * max * t[i])) /
        ((max - min) * (mode - min) * (max - mode) * pow(t[i], 2));
      rc.r = cc.real();
      rc.i = cc.imag();
      c[i] = rc;
    }
  }

  if (has_nan)
  {
    warning("NaN(s) produced.");
  }

  return c;
}

// [[Rcpp::export]]
ComplexVector CTriC2(
    NumericVector t, NumericVector min, NumericVector max, NumericVector mode)
{
  int n = t.size();
  bool has_nan = false;
  complex<double> x(0.0, 1.0);
  ComplexVector c(n);
  Rcomplex rc;

  for (int i = 0; i < n; i++)
  {
    if (t[i] == 0.0 || min[i] >= max[i] || mode[i] >= max[i] ||
        min[i] >= mode[i])
    {
      rc.r = R_NaN;
      rc.i = 0.0;
      c[i] = rc;
      has_nan = true;
    }
    else
    {
      complex<double> cc = -2.0 * ((max[i] - mode[i]) * exp(x * min[i] * t[i]) - (max[i] - min[i]) * exp(x * mode[i] * t[i]) + (mode[i] - min[i]) * exp(x * max[i] * t[i])) /
        ((max[i] - min[i]) * (mode[i] - min[i]) * (max[i] - mode[i]) *
          pow(t[i], 2));
      rc.r = cc.real();
      rc.i = cc.imag();
      c[i] = rc;
    }
  }

  if (has_nan)
  {
    warning("NaN(s) produced.");
  }

  return c;
}

// [[Rcpp::export]]
NumericVector ESTriC(
    NumericVector p, double min, double max, double mode, bool lower_tail,
    bool log_p)
{
  int n = p.size();

  if (min >= max || mode > max || min > mode)
  {
    warning("NaN(s) produced.");
    return NumericVector(n, R_NaN);
  }

  bool has_nan = false;
  NumericVector es(n);

  for (int i = 0; i < n; i++)
  {
    if (log_p)
    {
      p[i] = exp(p[i]);
    }

    if (!lower_tail)
    {
      p[i] = 1.0 - p[i];
    }

    if (p[i] == 0.0 || p[i] < 0.0 || p[i] > 1.0)
    {
      es[i] = R_NaN;
      has_nan = true;
    }
    else if (p[i] < (mode - min) / (max - min))
    {
      es[i] = ((p[i] * min) + (2.0 / 3.0) * sqrt((max - min) * (mode - min)) *
        pow(p[i], 1.5)) /
          p[i];
    }
    else // if (p[i] >= (mode - min) / (max - min))
    {
      double b = (mode - min) / (max - min);
      es[i] = ((b * min) + (2.0 / 3.0) * sqrt((max - min) * (mode - min)) * pow(b, 1.5) + (((p[i] * max) + (2.0 / 3.0) * sqrt((max - min) * (max - mode)) * pow((1.0 - p[i]), 1.5)) - ((b * max) + (2.0 / 3.0) * sqrt((max - min) * (max - mode)) * pow((1.0 - b), 1.5)))) / p[i];
    }
  }

  if (has_nan)
  {
    warning("NaN(s) produced.");
  }

  return es;
}

// [[Rcpp::export]]
NumericVector ESTriC2(
    NumericVector p, NumericVector min, NumericVector max, NumericVector mode,
    bool lower_tail, bool log_p)
{
  int n = p.size();
  bool has_nan = false;
  NumericVector es(n);

  for (int i = 0; i < n; i++)
  {
    if (log_p)
    {
      p[i] = exp(p[i]);
    }

    if (!lower_tail)
    {
      p[i] = 1.0 - p[i];
    }

    if (min[i] >= max[i] || mode[i] > max[i] || min[i] > mode[i] ||
        p[i] <= 0.0 || p[i] > 1.0)
    {
      es[i] = R_NaN;
      has_nan = true;
    }
    else if (p[i] < (mode[i] - min[i]) / (max[i] - min[i]))
    {
      es[i] = ((p[i] * min[i]) + (2.0 / 3.0) * sqrt((max[i] - min[i]) * (mode[i] - min[i])) * pow(p[i], 1.5)) / p[i];
    }
    else // if (p[i] >= (mode - min) / (max - min))
    {
      double b = (mode[i] - min[i]) / (max[i] - min[i]);
      es[i] = ((b * min[i]) + (2.0 / 3.0) * sqrt((max[i] - min[i]) * (mode[i] - min[i])) * pow(b, 1.5) + (((p[i] * max[i]) + (2.0 / 3.0) * sqrt((max[i] - min[i]) * (max[i] - mode[i])) * pow((1.0 - p[i]), 1.5)) - ((b * max[i]) + (2.0 / 3.0) * sqrt((max[i] - min[i]) * (max[i] - mode[i])) * pow((1.0 - b), 1.5)))) / p[i];
    }
  }

  if (has_nan)
  {
    warning("NaN(s) produced.");
  }

  return es;
}
