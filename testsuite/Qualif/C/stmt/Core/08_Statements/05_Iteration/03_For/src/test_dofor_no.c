#include "dofor.h"

int
main (void)
{
  int cov = 0;

  /* Never call "doif", but reference it anyway so that it is included by the
     linker (gnatcov will not cover it otherwise).  */
  if (cov)
    dofor (0, 0);
  return 0;
}

//# dofor.c
//  /body/      l- ## s-
//  /goto-in/   l- ## s-
//  /eval/      l- ## s-
//  /for/       l- ## s-
//  /goto-out/  l- ## s-
