#include "pkg.h"

int
dead_after_continue_in_do (int n)
{
  int t = 0;
  int i = 0;
  do
    {
      i++;
      continue;
      t += 100;
    }
  while (i < n);
  return t;
}
