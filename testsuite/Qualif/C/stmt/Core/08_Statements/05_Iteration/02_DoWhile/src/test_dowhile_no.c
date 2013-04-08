#include "dowhile.h"

int
main (void)
{
  int cov = 0;

  /* Never call "doif", but reference it anyway so that it is included by the
     linker (gnatcov will not cover it otherwise).  */
  if (cov)
    dowhile (0, 0);
  return 0;
}

//# dowhile.c
//  /body/      l- ## s-
//  /goto-in/   l- ## s-
//  /pre-while/ l- ## s-
//  /eval/      l- ## s-
//  /while/     l- ## s-
//  /goto-out/  l- ## s-
