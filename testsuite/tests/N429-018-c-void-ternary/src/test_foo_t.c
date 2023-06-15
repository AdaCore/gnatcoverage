#include "foo.h"

int
main (void)
{
  foo (1);
  return 0;
}

//# foo.c
//  /if-expr/        l+ ## 0
//  /if-then/        l+ ## 0
//  /if-else/        l- ## s-
//  /tern-void-stmt/ l! ## s-
//  /tern-int-stmt/  l+ ## 0
//  /tern-void-expr/ l+ ## 0
