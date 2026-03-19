#include "dowhile.h"

int cov = 0;

int
main (void)
{
  /* Never call "doif", but reference it anyway so that it is included by the
     linker (gnatcov will not cover it otherwise).  */
  if (cov)
    dowhile (0, 0);
  return 0;
}

//# dowhile.c
//  /body/          l- ## s-
//  /goto-in/       l- ## s-
//  /eval/          l- ## s-
//  /while-eval/    l- ## s-
//  /while-in/      l- ## s-
//  /goto-out/      l- ## s-
