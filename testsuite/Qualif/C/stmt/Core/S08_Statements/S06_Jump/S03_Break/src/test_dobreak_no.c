#include "dobreak.h"

int cov = 0;

int
main (void)
{
  /* Never call "dobreak", but reference it anyway so that it is included by
     the linker (gnatcov will not cover it otherwise).  */
  if (cov)
    dobreak (0, 0);
  return 0;
}

//# dobreak.c
//  /body/          l- ## s-
//  /while/         l- ## s-
//  /break-soft/    l- ## s-
//  /break-hard/    l- ## s-
