#include "pkg.h"

int
main (void)
{
  A a (5); // # var-decl

  a            // # var-ref
    .get_x (); // # method-call

  /* GNATCOV_DUMP_BUFFERS */
}

//# test_simple_call.cpp
// /var-decl/    l+ ## 0
// /var-ref/     l+ ## 0
// /method-call/ l+ ## 0
