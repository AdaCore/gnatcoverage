#include "foo.h"

int
ident (int x)
{
  if (x == 0)
    return -1;
  return static_helper (&x);
}
