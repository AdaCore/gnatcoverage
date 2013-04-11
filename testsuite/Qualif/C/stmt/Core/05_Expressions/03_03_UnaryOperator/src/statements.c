#include "statements.h"

int
run_statements (int full, int arg)
{
  int *b = &arg;    // # statements-aux-all

  &arg;             // # statements-all
  *b;               // # statements-all
  +arg;             // # statements-all
  -arg;             // # statements-all
  ~arg;             // # statements-all
  !arg;             // # statements-all

  if (full)         // # statements-aux-all
    {
      &arg;         // # statements-cond
      *b;           // # statements-cond
      +arg;         // # statements-cond
      -arg;         // # statements-cond
      ~arg;         // # statements-cond
      !arg;         // # statements-cond
      /* The following statement is needed so that the previous statements are
         considered as covered even if they generate no code.  */
      ++arg;        // # statements-aux-cond
    }

  return arg;       // # statements-aux-all
}
