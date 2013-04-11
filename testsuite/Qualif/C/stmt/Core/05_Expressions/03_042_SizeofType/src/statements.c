#include "statements.h"

int
run_statements (int foo, int arg)
{
  sizeof (int);     // # statements-all
  if (foo)          // # statements-aux-all
    {
      sizeof (int); // # statements-cond
      /* The following statement is needed so that the previous statement is
         considered as covered even if it generates no code.  */
      arg += 1;     // # statements-aux-cond
    }
  return arg;       // # statements-aux-all
}
