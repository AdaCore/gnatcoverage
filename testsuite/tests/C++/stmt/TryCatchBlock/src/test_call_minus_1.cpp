#include "pkg.h"

int
main (void)
{
  catch_not_positive (-1);
}

//# pkg.cpp
//
// /if/            l+ ## 0
// /throw_1/       l- ## s-
// /return_val/    l- ## s-
//
// /return_2/         l+ ## 0
// /return_[13]/      l- ## s-
//
// /call_validate_1/  l+ ## 0
// /call_validate_2/  l- ## s-
//
// %tags:block
// =/call_validate_1/ l- ## s-
