#include "statements.h"

void
run_statements (int full)
{
  if (full)             // # statements-aux-all
    {
      goto compound_in; // # statements-cond

      {
        full = 0;

compound_in: ;
        int a = 0;      // # statements-cond

        a += 1;         // # statements-cond
      }
    }
}
