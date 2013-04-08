#include "statements.h"

struct foo
{
  int bar;
};

static struct foo foo_object = { 1 };

static struct foo
get_foo (void)
{
  return foo_object;    // # statements-aux-all
}

static struct foo *
get_foo_p (void)
{
  return &foo_object;   // # statements-aux-all
}


void
run_statements (int full)
{
  get_foo ().bar;       // # statements-all
  if (full)             // # statements-aux-all
    get_foo ().bar;     // # statements-cond

  get_foo_p ()->bar;    // # statements-all
  if (full)             // # statements-aux-all
    get_foo_p ()->bar;  // # statements-cond
}
