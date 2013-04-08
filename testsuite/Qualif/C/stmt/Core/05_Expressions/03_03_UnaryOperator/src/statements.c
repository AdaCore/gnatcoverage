#include "statements.h"

void
run_statements (int full)
{
  int a = 0;    // # statements-aux-all
  int *b = &a;  // # statements-aux-all

  &a;           // # statements-all
  *b;           // # statements-all
  +a;           // # statements-all
  -a;           // # statements-all
  ~a;           // # statements-all
  !a;           // # statements-all

  if (full)     // # statements-aux-all
    {
      &a;       // # statements-cond
      *b;       // # statements-cond
      a + *b;   // # statements-cond
      a - *b;   // # statements-cond
      ~a;       // # statements-cond
      !a;       // # statements-cond
      /* The following statement is needed so that the previous statement is
         considered as covered even if it generates no code.  */
      a += 1;   // # statements-aux-cond
    }
}
