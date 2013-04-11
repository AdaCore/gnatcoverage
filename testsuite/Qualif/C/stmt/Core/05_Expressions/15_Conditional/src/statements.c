#include "statements.h"

static int
incr (int *arg)
{
  return ++*arg;                        // # statements-aux-all
}

static int
decr (int *arg)
{
  return --*arg;                        // # statements-aux-all
}

int
run_statements (int full, int arg)
{
  /* Call zero and one so that they are covered iff. statements-aux-all must be
     covered.  */
  incr (&arg);                          // # statements-aux-all
  decr (&arg);                          // # statements-aux-all

  full ? incr (&arg) : decr (&arg);     // # statements-all
  if (full)                             // # statements-aux-all
    full ? incr (&arg) : decr (&arg);   // # statements-cond

  return arg;                           // # statements-aux-all
}
