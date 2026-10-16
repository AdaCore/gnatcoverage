#include "foo.h"

int
main (void)
{
  /* Prevent the linker from optimize away "bar_true".  */
  if (identity (0))
    bar_true ();
  bar_false ();
  return 0;
}

//# bar_true.c
//  /bar-true/       l- ## s-
//# bar_false.c
//  /bar-false/      l+ ## 0

//# foo.c
//  /foo-exercised/  l+ ## 0

//# foo.h
//  /if-expr/        s=>l+, dmu=>l! ## s=>0, dmu=>dT-
//  /if-then/        l- ## s-
//  /if-else/        l+ ## 0
