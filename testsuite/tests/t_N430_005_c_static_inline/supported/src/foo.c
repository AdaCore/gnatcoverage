#include "foo.h"

static int current = 0;
static int processes[10];

void
set_current (int p)
{
  current = p; // # foo-exercised
}

int
get_current (void)
{
  return current; // # foo-exercised
}

int *
get_process (int p)
{
  return &processes[p]; // # foo-exercised
}
