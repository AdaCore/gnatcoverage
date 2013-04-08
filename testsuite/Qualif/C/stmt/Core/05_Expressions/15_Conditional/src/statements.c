#include "statements.h"

static int
zero (void)
{
  return 0;                     // # statements-aux-all
}

static int
one (void)
{
  return 1;                     // # statements-aux-all
}

void
run_statements (int full)
{
  int a = 0;                    // # statements-aux-all

  /* Call zero and one so that they are covered iff. statements-aux-all must be
     covered.  */
  zero ();                      // # statements-aux-all
  one ();                       // # statements-aux-all

  full ? zero () : one ();      // # statements-all
  if (full)                     // # statements-aux-all
    full ? zero () : one ();    // # statements-cond
}
