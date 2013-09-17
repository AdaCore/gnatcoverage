#include "dofor.h"

int cov = 0;

int
main (void)
{
  /* Never call "doif", but reference it anyway so that it is included by the
     linker (gnatcov will not cover it otherwise).  */
  if (cov)
    dofor (0, 0);
  return 0;
}

//# dofor.c
//  /body/      l- ## s-
//  /goto-in/   l- ## s-
//  /pre-for/   l- ## s-
//  /eval/      l- ## s-, s-
//  /for/       l- ## s-
//  /goto-out/  l- ## s-
//  /return/    l- ## s-
