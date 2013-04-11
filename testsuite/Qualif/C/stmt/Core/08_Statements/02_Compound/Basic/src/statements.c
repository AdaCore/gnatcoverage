#include "statements.h"

int
run_statements (int full, int arg)
{
  {
    if (!full)          // # statements-aux-all
      {
        --arg;          // # statements-not-cond
        goto middle;    // # statements-not-cond
      }
    ++arg;              // # statements-cond
  }

middle:
  if (!full)            // # statements-aux-all
    {
      --arg;            // # statements-not-cond
      goto end;         // # statements-not-cond
    }

  {
    ++arg;              // # statements-cond
  }

end:
  return arg;           // # statements-aux-all
}
