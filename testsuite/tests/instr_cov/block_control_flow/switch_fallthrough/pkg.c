#include "pkg.h"

int
fallthrough (int n)
{
  int t = 0;
  switch (n)
    {
    case 1:
      t += 1;
      t += 2;
    case 2:
      t += 4;
      break;
    default:
      t += 8;
    }
  return t;
}
