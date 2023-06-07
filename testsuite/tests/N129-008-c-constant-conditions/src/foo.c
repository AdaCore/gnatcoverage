#include "foo.h"

int
foo (int a, int b)
{
  if (((long int) (sizeof (long long))) < 0) // # cond-expr
    return a;                                // # cond-true
  else
    return b; // # cond-false
}
