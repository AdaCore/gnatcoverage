#include "fact.h"

static int
f (int n)
{
  if (n <= 1) // # fact-partial
    return 1; // # fact-partial
  else
    return n * f (n - 1); // # fact-full
}

int
compute_fact (int n)
{
  return f (n); // # fact-partial
}
