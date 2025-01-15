#include "foo.h"
#include "helpers.h"

int
main (void)
{
  /* Prevent the linker from optimize away "foo".  */
  if (identity (0))
    foo (0);
  return 0;
}

//# foo.c
//  /if-expr/        l- ## s-
//  /if-then/        l- ## s-
//  /if-else/        l- ## s-
//  /tern-void-stmt/ l- ## s-, s-, s-
//  /tern-int-stmt/  l- ## s-
//  /tern-void-expr/ l- ## s-
