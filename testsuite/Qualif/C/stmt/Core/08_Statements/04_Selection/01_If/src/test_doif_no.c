#include "doif.h"

#define NULL 0

int cov = 0;

int
main (void)
{
  /* Never call "doif", but reference it anyway so that it is included by the
     linker (gnatcov will not cover it otherwise). */
  if (cov)
    doif (0, NULL);
  return 0;
}

//# doif.c
//  /body/  l- ## s-
//  /eval/  l- ## s-
//  /if/    l- ## s-
//  /else/  l- ## s-
