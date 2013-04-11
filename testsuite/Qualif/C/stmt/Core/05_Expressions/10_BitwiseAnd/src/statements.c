#include "statements.h"

int
run_statements (int full, int arg)
{
  ++arg & 0;        // # statements-all
  if (full)         // # statements-aux-all
    ++arg & 0;      // # statements-cond

  return arg;       // # statements-aux-all
}
