#include "statements.h"

int
run_statements (int full, int arg)
{
  ++arg << 1;       // # statements-all
  if (full)         // # statements-aux-all
    ++arg << 1;     // # statements-cond

  ++arg >> 1;       // # statements-all
  if (full)         // # statements-aux-all
    ++arg >> 1;     // # statements-cond

  return arg;       // # statements-aux-all
}
