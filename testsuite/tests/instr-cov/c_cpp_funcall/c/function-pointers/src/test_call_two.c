#include "pkg.h"

int
main (void)
{
  foo (add_two);
  return 0;
}

//# pkg.c
// /add_one_def_1/ l- ## f-
// /add_one_def_2/ l- ## 0
// /return_one/    l- ## s-
