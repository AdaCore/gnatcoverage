#include "statements.h"

void
run_statements (int full)
{
  int a = 0;        // # statements-aux-all

  a ^ 0;            // # statements-all
  if (full)         // # statements-aux-all
    {
      a ^ 0;        // # statements-cond
      /* The following statement is needed so that the previous statement is
         considered as covered even if it generates no code.  */
      a += 1;       // # statements-aux-cond
    }
}
