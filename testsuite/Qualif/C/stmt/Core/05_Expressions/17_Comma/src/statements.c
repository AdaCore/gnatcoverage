#include "statements.h"

int
run_statements (int full, int arg)
{
  arg -= 1, arg += 2;   // # statements-all
  if (full)             // # statements-aux-all
    arg -= 1, arg += 2; // # statements-cond

  return arg;           // # statements-aux-all
}
