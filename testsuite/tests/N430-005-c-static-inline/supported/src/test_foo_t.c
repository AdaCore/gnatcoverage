#include "foo.h"

int
main (void)
{
  /* Prevent the linker from optimize away "bar_false".  */
  if (identity (0))
    bar_false ();
  bar_true ();
  return 0;
}

//# bar_true.c
//  /bar-true/       l+ ## 0
//# bar_false.c
//  /bar-false/      l- ## s-

//# foo.c
//  /foo-exercised/  l+ ## 0

//# foo.h
//  /if-expr/        s=>l+, dmu=>l! ## s=>0, dmu=>dF-
//  /if-then/        l+ ## 0
//  /if-else/        l- ## s-
