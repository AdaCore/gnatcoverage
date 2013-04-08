#include "statements.h"

void
run_statements (int full)
{
  int a = 0;    // # statements-aux-all

  ++a;          // # statements-all
  --a;          // # statements-all
  if (full)     // # statements-aux-all
    {
      ++a;      // # statements-cond
      --a;      // # statements-cond
    }
}
