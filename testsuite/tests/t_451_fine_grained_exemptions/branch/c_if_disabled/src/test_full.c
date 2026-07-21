#include "ops.h"

int
main (void)
{
  process (false);
  process (true);
  return 0;
}

//# ops.c
//
// /disabled/     lD ## 0
// /print_1/      l+ ## 0
// /exempt_print/ l# ## x0:"J2"
