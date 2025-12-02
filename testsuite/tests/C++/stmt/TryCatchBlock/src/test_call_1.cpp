#include "pkg.h"

int
main (void)
{
  catch_not_positive (1);
}

//# pkg.cpp
//
// /if/            l+ ## 0
// /throw/         l- ## s-
// /return_val/    l+ ## 0
//
// /call_validate/ l+ ## 0
// /return_[12]/   l- ## s-
// /return_3/      l+ ## 0
