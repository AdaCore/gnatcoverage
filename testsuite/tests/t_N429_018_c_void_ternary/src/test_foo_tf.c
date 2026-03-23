#include "foo.h"

int
main (void)
{
  foo (0);
  foo (1);
  return 0;
}

//# foo.c
//  /if-expr/        l+ ## 0
//  /if-then/        l+ ## 0
//  /if-else/        l+ ## 0
//  /tern-void-stmt/ l+ ## 0
//  /tern-int-stmt/  l+ ## 0
//  /tern-void-expr/ l+ ## 0
