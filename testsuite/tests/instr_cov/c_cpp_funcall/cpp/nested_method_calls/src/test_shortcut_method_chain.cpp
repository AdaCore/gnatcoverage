#include "pkg.h"

int
main (void)
{
  A a (5); // # var-decl
  B b (a); // # var-decl

  // .get_a() exits the program with 0 status code.
  b            // # var-ref
    .get_a ()  // # method-call
    .get_x (); // # not-reached

  /* GNATCOV_DUMP_BUFFERS */
}

//# test_shortcut_method_chain.cpp
// /var-decl/    l+ ## 0
// /var-ref/     l+ ## 0
// /method-call/ l+ ## 0
// /not-reached/ l! ## c-
