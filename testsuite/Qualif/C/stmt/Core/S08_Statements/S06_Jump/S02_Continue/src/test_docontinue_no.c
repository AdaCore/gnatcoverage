#include "docontinue.h"

int cov = 0;

int
main (void)
{
  /* Never call "docontinue", but reference it anyway so that it is included by
     the linker (gnatcov will not cover it otherwise).  */
  if (cov)
    docontinue (0, 0);
  return 0;
}

//# docontinue.c
//  /body/          l- ## s-
//  /while/         l- ## s-
//  /continue-soft/ l- ## s-
//  /continue-hard/ l- ## s-
//  /not-continue/  l- ## s-
//  /eval/          l- ## s-
