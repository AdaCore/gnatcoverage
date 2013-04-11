#include "statements.h"

int
run_statements (int full, int arg)
{
  arg;          // # statements-all
  if (full)     // # statements-aux-all
    {
      arg;      // # statements-cond
      /* The following statement is needed so that the previous statement is
         considered as covered even if it generates no code.  */
      ++arg;    // # statements-aux-cond
    }

  return arg;   // # statements-aux-all
}
