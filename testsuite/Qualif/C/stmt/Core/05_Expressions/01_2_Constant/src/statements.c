#include "statements.h"

int
run_statements (int full, int arg)
{
  0x80;         // # statements-all
  if (full)     // # statements-aux-all
    {
      0xff;     // # statements-cond
      /* The following statement is needed so that the previous statement is
         considered as covered even if it generates no code.  */
      arg += 1; // # statements-aux-cond
    }

  return arg;   // # statements-aux-all
}
