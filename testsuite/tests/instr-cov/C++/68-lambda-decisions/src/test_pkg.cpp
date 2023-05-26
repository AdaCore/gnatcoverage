#include "pkg.hh"
#include <cassert>

int
main ()
{
  assert (overly_complex_fact (2) == 2);
  assert (overly_complex_fact (0) == 1);
}

//# pkg.cpp
//
// /top_level/ l+ ## 0
// /lambda/    l! ## dT-
