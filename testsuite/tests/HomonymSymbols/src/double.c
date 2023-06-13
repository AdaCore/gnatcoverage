#include "double.h"

static int
f (int n)
{
  return 2 * n; // # double-partial
}

int
compute_double (int n)
{
  return f (n); // # double-partial
}
