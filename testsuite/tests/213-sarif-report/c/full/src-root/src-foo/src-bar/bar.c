#include "bar.h"

bool
mcdc_violation (int a, int b)
{
  if ((a && !b) || a)
    {
      return true;
    }
  else
    {
      return true;
    }
}

bool
bar ()
{
  bool t = true;
  bool f = false;
  bool res = false;

  res = mcdc_violation (t, f);
  res = mcdc_violation (f, f);

  return res;
}
