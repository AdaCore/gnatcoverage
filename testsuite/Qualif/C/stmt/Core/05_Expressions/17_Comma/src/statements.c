#include "statements.h"

void
run_statements (int full)
{
  int a = 0;        // # statements-aux-all

  a = 1, a += 2;    // # statements-all
  if (full)         // # statements-aux-all
    a = 1, a += 2;  // # statements-cond
}
