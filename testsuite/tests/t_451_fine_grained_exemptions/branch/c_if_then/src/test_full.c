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
// /condition/    l# ## 0
// /exempt/       l# ## x0:"J"
// /exempt_print/ l= ## 0
// /print_2/      l+ ## 0
