#include "pkg.h"

int
dead_after_continue (int n)
{
  int t = 0;
  int i;
  for (i = 0; i < n; i++)
    {
      t += i;
      continue;
      t += 100;
    }
  return t;
}
