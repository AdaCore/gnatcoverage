#include "foo.h"

int
main (void)
{
  foo (1, 2);
  return 0;
}

//# foo.c

// For binary traces, as the if condition is constant, the then part will
// be no code, and there won't be a decision obligation.

//%opts: --trace-mode=bin
//  /cond-expr/    l+ ## 0
//  /cond-true/    l. ## 0
//  /cond-false/   l+ ## 0

//%opts: --trace-mode=src
//  /cond-true/    l- ## s-
//  /cond-false/   l+ ## 0

//%cov: --level=stmt %opts: --trace-mode=src
//  =/cond-expr/    l+ ## 0

//%cov: --level=stmt\+decision %opts: --trace-mode=src
//  =/cond-expr/    l! ## dT-

//%cov: --level=stmt\+(uc_)?mcdc %opts: --trace-mode=src
//  =/cond-expr/    l! ## dT-
