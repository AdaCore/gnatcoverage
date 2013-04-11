#include "statements.h"

static void
nop (int *arg)
{
  ++*arg;       // # statements-aux-all
}

int
run_statements (int full, int arg)
{
  nop (&arg);   // # statements-all
  if (full)     // # statements-aux-all
    nop (&arg); // # statements-cond

  return arg;   // # statements-aux-all
}
