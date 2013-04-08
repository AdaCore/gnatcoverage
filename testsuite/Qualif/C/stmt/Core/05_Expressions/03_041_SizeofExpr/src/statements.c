#include "statements.h"

void
run_statements (int foo)
{
  int a = 0;        // # statements-aux-all

  sizeof (a);       // # statements-all
  if (foo)          // # statements-aux-all
    {
      sizeof (a);   // # statements-cond
      /* The following statement is needed so that the previous statement is
         considered as covered even if it generates no code.  */
      a += 1;       // # statements-aux-cond
    }
}
