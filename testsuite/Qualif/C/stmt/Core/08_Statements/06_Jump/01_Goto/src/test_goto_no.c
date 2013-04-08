#include "dogoto.h"

int
main (void)
{
  int cov = 0;

  /* Never call "dogoto", but reference it anyway so that it is included by the
     linker (gnatcov will not cover it otherwise).  */
  if (cov)
    dogoto (0);
  return 0;
}

//# dogoto.c
//  /body/          l- ## s-
//  /unreachable/   l- ## s-
//  /cond/          l- ## s-
