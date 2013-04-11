#include "statements.h"

int
run_statements (int full, int arg)
{
  {
    if (!full)              // # statements-aux-all
      {
        ++arg;              // # statements-not-cond
        goto compound_out;  // # statements-not-cond
      }

    --arg;                  // # statements-cond
  }

compound_out:
  return arg;               // # statements-aux-all
}
