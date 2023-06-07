#include "cmp_internal.h"

int
less_than (int a, int b)
{
  int result; // # decl

  if (compute_less_than (&result, a, b), result) // # if-cond
    return 1;                                    // # if-then
  else
    return 0; // # if-else
}
