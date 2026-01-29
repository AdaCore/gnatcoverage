#include "pkg.h"

int
main (void)
{
  A a (5); // # var-decl
  B b (a); // # var-decl

  // .get_a() exits the program with 0 status code.
  b            // # var-ref
    .get_a (); // # method-call

  // Program should not reach this point.
}

//# test_exiting_method.cpp
// /var-decl/    l+ ## 0
// /var-ref/     l+ ## 0
// /method-call/ l+ ## 0
