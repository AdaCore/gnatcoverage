#include "sum.hh"

int
main (void)
{
  if (sum ({ 1, 2 }) != 3)
    return 1;
  return 0;
}

//# sum.cpp
//
//  /init/      l+ ## 0
//  /for-range/ l+ ## 0
//  /if-cond/   l+ ## 0
//  /if-return/ l- ## s-
//  /if-add/    l+ ## 0
//  /return/    l+ ## 0
