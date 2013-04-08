#include "statements.h"

void
run_statements (int full)
{
  int b = 0;                // # statements-aux-all

  {
    int a = 0;              // # statements-all

    if (!full)              // # statements-all
      goto compound_out;    // # statements-not-cond

    a += 1;                 // # statements-cond
  }

compound_out:
  ;
}
