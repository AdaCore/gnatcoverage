#include "foo.h"

int
foo (int a, int b)
{
  /* Currently, the following expression is folded by GCC's C frontend into:
        !(a == 0) || (b == 0)
     Because of this, SCOs won't match sources and thus reported violations
     will not be the expected ones.  */
  return (a == 0) ? (b == 0) : 1; // # decision
}
