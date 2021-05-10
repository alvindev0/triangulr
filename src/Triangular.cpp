#include "cpp11.hpp"
using namespace cpp11;
namespace writable = cpp11::writable;

[[cpp11::register]]
doubles dtri_cpp(doubles x, double min, double max, double mode, bool log)
{
  int n = x.size();
  writable::doubles d(n);

  if (min >= max || mode > max || min > mode)
  {
    for (int i = 0; i < n; i++)
    {
      d[i] = NA_REAL;
    }

    cpp11::warning("NaN(s) produced.");

    return d;
  }

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

    // TODO: This is inefficient
    if (log)
    {
      d[i] = std::log(d[i]);
    }
  }

  return d;
}

[[cpp11::register]]
doubles dtri_cpp2(doubles x, doubles min, doubles max, doubles mode, bool log)
{
  int n = x.size();
  bool has_nan = false;
  writable::doubles d(n);

  for (int i = 0; i < n; i++)
  {
    if (min[i] >= max[i] || mode[i] > max[i] || min[i] > mode[i])
    {
      d[i] = NA_REAL;
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

    // TODO: This is inefficient
    if (log)
    {
      d[i] = std::log(d[i]);
    }
  }

  if (has_nan)
  {
    cpp11::warning("NaN(s) produced.");
  }

  return d;
}

[[cpp11::register]]
doubles ptri_cpp(
    doubles q, double min, double max, double mode, bool lower_tail, bool log_p)
{
  int n = q.size();
  writable::doubles p(n);

  if (min >= max || mode > max || min > mode)
  {
    for (int i = 0; i < n; i++)
    {
      p[i] = NA_REAL;
    }

    cpp11::warning("NaN(s) produced.");

    return p;
  }

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

    // TODO: This is inefficient
    if (!lower_tail)
    {
      p[i] = 1.0 - p[i];
    }

    // TODO: This is inefficient
    if (log_p)
    {
      p[i] = std::log(p[i]);
    }
  }

  return p;
}

[[cpp11::register]]
doubles ptri_cpp2(
    doubles q, doubles min, doubles max, doubles mode, bool lower_tail,
    bool log_p)
{
  int n = q.size();
  bool has_nan = false;
  writable::doubles p(n);

  for (int i = 0; i < n; i++)
  {
    if (min[i] >= max[i] || mode[i] > max[i] || min[i] > mode[i])
    {
      p[i] = NA_REAL;
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

    // TODO: This is inefficient
    if (!lower_tail)
    {
      p[i] = 1.0 - p[i];
    }

    // TODO: This is inefficient
    if (log_p)
    {
      p[i] = std::log(p[i]);
    }
  }

  if (has_nan)
  {
    cpp11::warning("NaN(s) produced.");
  }

  return p;
}
