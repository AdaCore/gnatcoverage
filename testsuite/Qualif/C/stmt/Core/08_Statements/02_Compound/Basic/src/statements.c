#include "statements.h"

void
run_statements (int full)
{
  {
    int a = 0;          // # statements-all

    if (!full)          // # statements-aux-all
      goto middle;      // # statements-not-cond
    a += 1;             // # statements-cond
  }

middle:
  if (!full)            // # statements-aux-all
    goto end;           // # statements-not-cond
  {
    int a = 0;          // # statements-cond
    a += 1;             // # statements-cond
  }

end:
  ;
}
