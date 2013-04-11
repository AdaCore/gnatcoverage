#include "statements.h"

int
run_statements (int foo, int arg)
{
  sizeof (arg);     // # statements-all
  if (foo)          // # statements-aux-all
  {
      sizeof (arg); // # statements-cond
      ++arg;        // # statements-aux-cond
      /* The following statement is needed so that the previous statement is
         considered as covered even if it generates no code.  */
  }

  return arg;       // # statements-aux-all
}
