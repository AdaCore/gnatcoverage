#include "foo.h"

int
main (int argc, char *argv[])
{
  foo (0, 0);
  foo (1, 0);
  return 0;
}

//# foo.c
//%cov: --level=stmt
//  =/decision/   l+ ## 0
//%cov: --level=stmt\+decision
//  =/decision/   l+ ## 0
//%cov: --level=stmt\+(uc_)?mcdc
//  =/decision/   l+ ## 0
