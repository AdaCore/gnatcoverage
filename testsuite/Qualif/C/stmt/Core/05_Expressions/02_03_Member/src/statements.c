#include "statements.h"

struct foo
{
  int bar;
};

static struct foo foo_objects[5] =
{
  { 1 },
  { 2 },
  { 3 },
  { 4 },
  { 5 },
};

struct foo
get_foo (int arg)
{
  foo_objects[arg].bar += 1;    // # statements-aux-all
  return foo_objects[arg];      // # statements-aux-all
}

struct foo *
get_foo_p (int arg)
{
  foo_objects[arg].bar += 1;    // # statements-aux-all
  return &foo_objects[arg];     // # statements-aux-all
}


int
run_statements (int full, int arg)
{
  get_foo (++arg).bar;          // # statements-all
  if (full)                     // # statements-aux-all
    get_foo (++arg).bar;        // # statements-cond

  get_foo_p (++arg)->bar;       // # statements-all
  if (full)                     // # statements-aux-all
    get_foo_p (++arg)->bar;     // # statements-cond

  return arg;                   // # statements-aux-all
}
