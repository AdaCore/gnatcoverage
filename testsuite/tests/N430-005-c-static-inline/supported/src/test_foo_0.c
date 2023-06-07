#include "foo.h"

int
main (void)
{
  /* Prevent the linker from optimize away "bar_*".  */
  if (identity (0))
    {
      bar_true ();
      bar_false ();
    }
  else
    return 0;
}

//# bar_true.c
//  /bar-true/       l- ## s-
//# bar_false.c
//  /bar-false/      l- ## s-

//# foo.c
//  /foo-exercised/  l- ## s-

//# foo.h
//  /if-expr/        l- ## s-
//  /if-then/        l- ## s-
//  /if-else/        l- ## s-
