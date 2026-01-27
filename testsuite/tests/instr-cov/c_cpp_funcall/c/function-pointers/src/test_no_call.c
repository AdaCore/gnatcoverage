#include "pkg.h"

int
main (void)
{
  return 0;
}

//# pkg.c
// /.*def_1/       l- ## f-
// /.*def_2/       l- ## 0
// /.*call/        l- ## s-,c-
// /no-op/         l- ## s-
// /return_.*/     l- ## s-
// /fpointer_use/  l- ## c-
// /print_format/  l- ## 0
// /parenthesis/   l- ## 0
// /fpointer_ref/  l- ## 0
