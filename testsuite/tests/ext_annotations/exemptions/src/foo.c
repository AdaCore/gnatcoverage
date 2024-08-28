#include "foo.h"

int
ident (int x)
{
  if (x == 0)                // # exempt_c_d
    return -1;               // # exempt_c_s
  return static_helper (&x); // # ok
}
