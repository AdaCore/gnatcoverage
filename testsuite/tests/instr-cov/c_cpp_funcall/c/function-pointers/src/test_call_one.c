#include "pkg.h"

int
main (void)
{
  foo (add_one);
  return 0;
}

//# pkg.c
// /add_two_def_1/ l- ## f-
// /add_two_def_2/ l- ## 0
// /return_two/    l- ## s-
