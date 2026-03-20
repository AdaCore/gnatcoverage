#include "pkg.hh"

int
main ()
{
  if (!overly_complex_fact (2) == 2)
    return 1;
  if (!overly_complex_fact (0) == 1)
    return 1;
  return 0;
}

//# pkg.cpp
//
// /top_level/ l+ ## 0
// /lambda/    l! ## dT-
