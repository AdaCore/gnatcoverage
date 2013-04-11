#include "doswitch.h"

#define NULL 0

int cov = 0;

int
main (void)
{
  /* Never call "doif", but reference it anyway so that it is included by the
     linker (gnatcov will not cover it otherwise). */
  if (cov)
    doswitch (0, NULL);
  return 0;
}

//# doswitch.c
//  /body/      l- ## s-
//  /eval/      l- ## s-
//  /zero/      l- ## s-
//  /one/       l- ## s-
//  /two/       l- ## s-
//  /default/   l- ## s-
