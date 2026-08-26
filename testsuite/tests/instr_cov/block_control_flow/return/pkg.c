#include "pkg.h"

int
dead_after_return (int x)
{
  int r = x + 1;
  return r;
  r = 0;
}
