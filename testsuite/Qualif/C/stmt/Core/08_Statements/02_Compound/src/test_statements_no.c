#include "statements.h"

int
main (void)
{
  int cov = 0;

  /* Never call "run_statement", but reference it anyway so that it is
     included by the linker (gnatcov will not cover it otherwise).  */
  if (cov)
    run_statements (0);
  return 0;
}

//# statements.c
//  /statements-all/        l- ## s-
//  /statements-aux-all/    l- ## s-
//  /statements-cond/       l- ## s-
//  /statements-aux-cond/   l- ## s-
//  /statements-not-cond/   ~l- ## ~s-
