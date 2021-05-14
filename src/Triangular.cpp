#include "cpp11.hpp"
#include "Rmath.h"

using namespace cpp11;
namespace writable = cpp11::writable;

double dtri_cpp_internal(
    double x, double min, double max, double mode, bool is_log)
{
  double d;
  if (x < min || x > max)
  {
    d = 0.0;
  }
  else if (min <= x && x < mode)
  {
    d = 2.0 * (x - min) / ((max - min) * (mode - min));
  }
  else if (x == mode)
  {
    d = 2.0 / (max - min);
  }
  else // if (mode < x && x <= max)
  {
    d = 2.0 * (max - x) / ((max - min) * (max - mode));
  }

  return is_log ? log(d) : d;
}

[[cpp11::register]]
doubles dtri_cpp(
    doubles x, doubles min, doubles max, doubles mode, bool is_log,
    bool is_scalar)
{
  int n = x.size();
  writable::doubles d(n);

  if (is_scalar)
  {
    if (min[0] >= max[0] || mode[0] > max[0] || min[0] > mode[0])
    {
      for (int i = 0; i < n; i++)
      {
        d[i] = NA_REAL;
      }

      warning("NaN(s) produced.");

      return d;
    }

    for (int i = 0; i < n; i++)
    {
      d[i] = dtri_cpp_internal(x[i], min[0], max[0], mode[0], is_log);
    }
  }
  else
  {
    bool has_nan = false;

    for (int i = 0; i < n; i++)
    {
      if (min[i] >= max[i] || mode[i] > max[i] || min[i] > mode[i])
      {
        d[i] = NA_REAL;
        has_nan = true;
      }
      else
      {
        d[i] = dtri_cpp_internal(x[i], min[i], max[i], mode[i], is_log);
      }
    }

    if (has_nan) warning("NaN(s) produced.");
  }

  return d;
}

double ptri_cpp_internal(
    double q, double min, double max, double mode, bool is_lower_tail,
    bool is_log_p)
{
  double p;

  if (q <= min)
  {
    p = 0.0;
  }
  else if (min < q && q <= mode)
  {
    p = pow((q - min), 2) / ((max - min) * (mode - min));
  }
  else if (mode < q && q < max)
  {
    p = 1.0 - pow((max - q), 2) / ((max - min) * (max - mode));
  }
  else // if (max <= q)
  {
    p = 1.0;
  }

  if (!is_lower_tail) p = 1.0 - p;

  return is_log_p ? log(p) : p;
}

[[cpp11::register]]
doubles ptri_cpp(
    doubles q, doubles min, doubles max, doubles mode, bool is_lower_tail,
    bool is_log_p, bool is_scalar)
{
  int n = q.size();
  writable::doubles p(n);

  if (is_scalar)
  {
    if (min[0] >= max[0] || mode[0] > max[0] || min[0] > mode[0])
    {
      for (int i = 0; i < n; i++)
      {
        p[i] = NA_REAL;
      }

      warning("NaN(s) produced.");

      return p;
    }

    for (int i = 0; i < n; i++)
    {
      p[i] = ptri_cpp_internal(
        q[i], min[0], max[0], mode[0], is_lower_tail, is_log_p);
    }
  }
  else
  {
    bool has_nan = false;

    for (int i = 0; i < n; i++)
    {
      if (min[i] >= max[i] || mode[i] > max[i] || min[i] > mode[i])
      {
        p[i] = NA_REAL;
        has_nan = true;
      }
      else
      {
        p[i] = ptri_cpp_internal(
          q[i], min[i], max[i], mode[i], is_lower_tail, is_log_p);
      }
    }

    if (has_nan) warning("NaN(s) produced.");
  }

  return p;
}

[[cpp11::register]]
doubles qtri_cpp(
    doubles p, double min, double max, double mode, bool lower_tail, bool log_p)
{
  int n = p.size();
  writable::doubles q(n);

  if (min >= max || mode > max || min > mode)
  {
    for (int i = 0; i < n; i++)
    {
      q[i] = NA_REAL;
    }

    cpp11::warning("NaN(s) produced.");

    return q;
  }

  bool has_nan = false;
  double int_len = max - min;

  for (int i = 0; i < n; i++)
  {
    // TODO: This is inefficient
    if (log_p)
    {
      q[i] = std::exp(p[i]);
    }
    else
    {
      q[i] = p[i];
    }

    // TODO: This is inefficient
    if (!lower_tail)
    {
      q[i] = 1.0 - q[i];
    }

    if (q[i] < 0.0 || q[i] > 1.0)
    {
      q[i] = NA_REAL;
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
    cpp11::warning("NaN(s) produced.");
  }

  return q;
}

[[cpp11::register]]
doubles qtri_cpp2(
    doubles p, doubles min, doubles max, doubles mode, bool lower_tail,
    bool log_p)
{
  int n = p.size();
  writable::doubles q(n);
  bool has_nan = false;

  for (int i = 0; i < n; i++)
  {
    // TODO: This is inefficient
    if (log_p)
    {
      q[i] = std::exp(p[i]);
    }
    else
    {
      q[i] = p[i];
    }

    // TODO: This is inefficient
    if (!lower_tail)
    {
      q[i] = 1.0 - q[i];
    }

    if (min[i] >= max[i] || mode[i] > max[i] || min[i] > mode[i] ||
        q[i] < 0.0 || q[i] > 1.0)
    {
      q[i] = NA_REAL;
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
    cpp11::warning("NaN(s) produced.");
  }

  return q;
}

[[cpp11::register]]
doubles rtri_cpp(int n, double min, double max, double mode)
{
  writable::doubles r(n);

  if (min >= max || mode > max || min > mode)
  {
    for (int i = 0; i < n; i++)
    {
      r[i] = NA_REAL;
    }

    cpp11::warning("NaN(s) produced.");

    return r;
  }

  double int_len = max - min;

  for (int i = 0; i < n; i++)
  {
    r[i] = unif_rand();

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

[[cpp11::register]]
doubles rtri_cpp2(int n, doubles min, doubles max, doubles mode)
{
  writable::doubles r(n);
  bool has_nan = false;

  for (int i = 0; i < n; i++)
  {
    if (min[i] >= max[i] || mode[i] > max[i] || min[i] > mode[i])
    {
      r[i] = NA_REAL;
      has_nan = true;
    }
    else
    {
      r[i] = unif_rand();

      if (r[i] < (mode[i] - min[i]) / (max[i] - min[i]))
      {
        r[i] = min[i] + sqrt(r[i] * (max[i] - min[i]) * (mode[i] - min[i]));
      }
      else // if (r[i] >= (mode[i] - min[i]) / (max[i] - min[i]))
      {
        r[i] = max[i] - sqrt((1.0 - r[i]) * (max[i] - min[i]) *
          (max[i] - mode[i]));
      }
    }
  }

  if (has_nan)
  {
    cpp11::warning("NaN(s) produced.");
  }

  return r;
}

[[cpp11::register]]
doubles mgtri_cpp(doubles t, double min, double max, double mode)
{
  int n = t.size();
  writable::doubles m(n);

  if (min >= max || mode >= max || min >= mode)
  {
    for (int i = 0; i < n; i++)
    {
      m[i] = NA_REAL;
    }

    warning("NaN(s) produced.");

    return m;
  }

  bool has_nan = false;

  for (int i = 0; i < n; i++)
  {
    if (t[i] == 0.0)
    {
      m[i] = NA_REAL;
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
    cpp11::warning("NaN(s) produced.");
  }

  return m;
}

[[cpp11::register]]
doubles mgtri_cpp2(doubles t, doubles min, doubles max, doubles mode)
{
  int n = t.size();
  bool has_nan = false;
  writable::doubles m(n);

  for (int i = 0; i < n; i++)
  {
    if (t[i] == 0.0 ||
        min[i] >= max[i] || mode[i] >= max[i] || min[i] >= mode[i])
    {
      m[i] = NA_REAL;
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
    cpp11::warning("NaN(s) produced.");
  }

  return m;
}

// NOTE: Will be implemented when cpp11 has complex vector class
// ComplexVector ctri_cpp(NumericVector t, double min, double max, double mode)
// {
//   int n = t.size();
//
//   if (min >= max || mode >= max || min >= mode)
//   {
//     warning("NaN(s) produced.");
//     return ComplexVector(n, complex<double>(R_NaN, 0.0));
//   }
//
//   bool has_nan = false;
//   complex<double> x(0.0, 1.0);
//   ComplexVector c(n);
//   Rcomplex rc;
//
//   for (int i = 0; i < n; i++)
//   {
//     if (t[i] == 0.0)
//     {
//       rc.r = R_NaN;
//       rc.i = 0.0;
//       c[i] = rc;
//       has_nan = true;
//     }
//     else
//     {
//       complex<double> cc = -2.0 * ((max - mode) * exp(x * min * t[i]) - (max - min) * exp(x * mode * t[i]) + (mode - min) * exp(x * max * t[i])) /
//         ((max - min) * (mode - min) * (max - mode) * pow(t[i], 2));
//       rc.r = cc.real();
//       rc.i = cc.imag();
//       c[i] = rc;
//     }
//   }
//
//   if (has_nan)
//   {
//     warning("NaN(s) produced.");
//   }
//
//   return c;
// }
//
// ComplexVector ctri_cpp2(
//     NumericVector t, NumericVector min, NumericVector max, NumericVector mode)
// {
//   int n = t.size();
//   bool has_nan = false;
//   complex<double> x(0.0, 1.0);
//   ComplexVector c(n);
//   Rcomplex rc;
//
//   for (int i = 0; i < n; i++)
//   {
//     if (t[i] == 0.0 || min[i] >= max[i] || mode[i] >= max[i] ||
//         min[i] >= mode[i])
//     {
//       rc.r = R_NaN;
//       rc.i = 0.0;
//       c[i] = rc;
//       has_nan = true;
//     }
//     else
//     {
//       complex<double> cc = -2.0 * ((max[i] - mode[i]) * exp(x * min[i] * t[i]) - (max[i] - min[i]) * exp(x * mode[i] * t[i]) + (mode[i] - min[i]) * exp(x * max[i] * t[i])) /
//         ((max[i] - min[i]) * (mode[i] - min[i]) * (max[i] - mode[i]) *
//           pow(t[i], 2));
//       rc.r = cc.real();
//       rc.i = cc.imag();
//       c[i] = rc;
//     }
//   }
//
//   if (has_nan)
//   {
//     warning("NaN(s) produced.");
//   }
//
//   return c;
// }

[[cpp11::register]]
doubles estri_cpp(
    doubles p, double min, double max, double mode, bool lower_tail, bool log_p)
{
  int n = p.size();
  writable::doubles es(n);

  if (min >= max || mode > max || min > mode)
  {
    for (int i = 0; i < n; i++)
    {
      es[i] = NA_REAL;
    }

    cpp11::warning("NaN(s) produced.");

    return es;
  }

  bool has_nan = false;

  for (int i = 0; i < n; i++)
  {
    if (log_p)
    {
      es[i] = std::exp(p[i]);
    }
    else
    {
      es[i] = p[i];
    }

    if (!lower_tail)
    {
      es[i] = 1.0 - es[i];
    }

    if (es[i] == 0.0 || es[i] < 0.0 || es[i] > 1.0)
    {
      es[i] = NA_REAL;
      has_nan = true;
    }
    else if (es[i] < (mode - min) / (max - min))
    {
      es[i] = ((es[i] * min) + (2.0 / 3.0) * sqrt((max - min) * (mode - min)) *
        pow(es[i], 1.5)) /
          es[i];
    }
    else // if (es[i] >= (mode - min) / (max - min))
    {
      double b = (mode - min) / (max - min);
      es[i] = ((b * min) + (2.0 / 3.0) * sqrt((max - min) * (mode - min)) * pow(b, 1.5) + (((es[i] * max) + (2.0 / 3.0) * sqrt((max - min) * (max - mode)) * pow((1.0 - es[i]), 1.5)) - ((b * max) + (2.0 / 3.0) * sqrt((max - min) * (max - mode)) * pow((1.0 - b), 1.5)))) / es[i];
    }
  }

  if (has_nan)
  {
    cpp11::warning("NaN(s) produced.");
  }

  return es;
}

[[cpp11::register]]
doubles estri_cpp2(
    doubles p, doubles min, doubles max, doubles mode, bool lower_tail,
    bool log_p)
{
  int n = p.size();
  bool has_nan = false;
  writable::doubles es(n);

  for (int i = 0; i < n; i++)
  {
    if (log_p)
    {
      es[i] = std::exp(p[i]);
    }
    else
    {
      es[i] = p[i];
    }

    if (!lower_tail)
    {
      es[i] = 1.0 - es[i];
    }

    if (min[i] >= max[i] || mode[i] > max[i] || min[i] > mode[i] ||
        es[i] <= 0.0 || es[i] > 1.0)
    {
      es[i] = NA_REAL;
      has_nan = true;
    }
    else if (es[i] < (mode[i] - min[i]) / (max[i] - min[i]))
    {
      es[i] = ((es[i] * min[i]) + (2.0 / 3.0) * sqrt((max[i] - min[i]) * (mode[i] - min[i])) * pow(es[i], 1.5)) / es[i];
    }
    else // if (es[i] >= (mode - min) / (max - min))
    {
      double b = (mode[i] - min[i]) / (max[i] - min[i]);
      es[i] = ((b * min[i]) + (2.0 / 3.0) * sqrt((max[i] - min[i]) * (mode[i] - min[i])) * pow(b, 1.5) + (((es[i] * max[i]) + (2.0 / 3.0) * sqrt((max[i] - min[i]) * (max[i] - mode[i])) * pow((1.0 - es[i]), 1.5)) - ((b * max[i]) + (2.0 / 3.0) * sqrt((max[i] - min[i]) * (max[i] - mode[i])) * pow((1.0 - b), 1.5)))) / es[i];
    }
  }

  if (has_nan)
  {
    cpp11::warning("NaN(s) produced.");
  }

  return es;
}
