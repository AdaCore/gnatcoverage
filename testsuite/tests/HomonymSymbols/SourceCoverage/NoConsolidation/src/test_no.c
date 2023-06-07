#include "fact.h"
#include "not.h"

int cov = 0;

int
main (void)
{
  /* Never call "run*", but reference then anyway so that they are included by
     the linker (gnatcov will not cover them otherwise).  */
  if (cov)
    {
      compute_fact (0);
      compute_not (0);
    }
  return 0;
}

//# fact.c
//  /fact-partial/  l- ## s-
//  /fact-full/     l- ## s-

//# not.c
//  /not-all/       l- ## s-
//  /not-true/      l- ## s-
//  /not-false/     l- ## s-
