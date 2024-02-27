#include "statements.h"

int
run_statements (int full, int arg)
{
  if (full) // # statements-aux-all
    {
      goto compound_in; // # statements-cond

      {
        arg = 1; // # statement-non-coverable

      compound_in:;
        ++arg; // # statements-cond
      }
    }
  return arg; // # statements-aux-all
}
