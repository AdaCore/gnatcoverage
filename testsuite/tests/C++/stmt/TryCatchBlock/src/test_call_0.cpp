#include "pkg.h"

int
main (void)
{
  catch_not_positive (0);
}

//# pkg.cpp
//
// /if_2/             l- ## s-
// /throw_2/          l- ## s-
// /return_val/       l- ## s-
//
// /return_1/         l+ ## 0
// /return_[23]/      l- ## s-
//
// /call_validate_1/  l+ ## 0
// /call_validate_2/  l- ## s-
//
// %tags:block
// =/call_validate_1/ l- ## s-
