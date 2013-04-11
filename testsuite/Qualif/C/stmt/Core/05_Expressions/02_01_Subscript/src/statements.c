#include "statements.h"

int
num (int *arg)
{
  return ++*arg;            // # statements-aux-all
}

int
run_statements (int full, int arg)
{
  int a[2] = { 0, 1, 2 };   // # statements-aux-all

  a[num (&arg)];            // # statements-all
  if (full)                 // # statements-aux-all
    a[num (&arg)];          // # statements-cond

  return arg;               // # statements-aux-all
}
