#include "statements.h"

static int
zero (void)
{
  return 0;     // # statements-aux-all
}

void
run_statements (int full)
{
  int a[1];     // # statements-aux-all

  a[zero ()];   // # statements-all
  if (full)     // # statements-aux-all
    a[zero ()]; // # statements-cond
}
