#include "pkg.h"

int
main ()
{
  int CONCAT (/*empty*/, my_var) = 0;         // # st
  return CONCAT (/*something else*/, my_var); // # st
}

//# test_pkg.c
//
// /st/ l+ ## 0

//# pkg.h
//
// /noeval/ l- ## s-
