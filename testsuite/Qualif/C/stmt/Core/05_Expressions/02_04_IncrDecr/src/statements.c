#include "statements.h"

int
run_statements (int full, int arg)
{
  arg++;        // # statements-all
  if (full)     // # statements-aux-all
    arg++;      // # statements-cond

  arg--;        // # statements-all
  if (full)     // # statements-aux-all
    arg--;      // # statements-cond
  return arg;   // # statements-aux-all
}
