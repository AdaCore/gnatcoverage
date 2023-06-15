#include "foo.h"

int
main (int argc, char *argv[])
{
  foo (0, 0);
  return 0;
}

//# foo.c
//%cov: --level=stmt
//  =/decision/   l+ ## 0
//%cov: --level=stmt\+decision %opts: --trace-mode=bin
//  =/decision/   l! ## d!
//%cov: --level=stmt\+decision %opts: --trace-mode=src
//  =/decision/   l! ## dF-
//%cov: --level=stmt\+(uc_)?mcdc %opts: --trace-mode=bin
//  =/decision/   l! ## d!
//%cov: --level=stmt\+(uc_)?mcdc %opts: --trace-mode=src
//  =/decision/   l! ## dF-
