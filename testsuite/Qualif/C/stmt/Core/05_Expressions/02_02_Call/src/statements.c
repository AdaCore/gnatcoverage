#include "statements.h"

static void
nop (void)
{
}

void
run_statements (int full)
{
  nop ();   // # statements-all
  if (full) // # statements-aux-all
    nop (); // # statements-cond
}
