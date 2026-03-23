#include "foo.h"

int
main (void)
{
  bar_true ();
  bar_false ();
  return 0;
}

//# bar_true.c
//  /bar-true/       l+ ## 0
//# bar_false.c
//  /bar-false/      l+ ## 0

//# foo.c
//  /foo-exercised/  l+ ## 0

//# foo.h
//  /if-expr/        l+ ## 0
//  /if-then/        l+ ## 0
//  /if-else/        l+ ## 0
