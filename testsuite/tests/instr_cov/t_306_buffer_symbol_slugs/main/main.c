#include "foo.h"

int
main (void)
{
  volatile int r = foo (1);
  return r == 2 ? 0 : 1;
}
