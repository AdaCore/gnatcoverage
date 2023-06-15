#include "sum.hh"

int
main (void)
{
  if (sum ({ 0, 1 }) != 0)
    return 1;
  return 0;
}

//# sum.cpp
//
//  /init/      l+ ## 0
//  /for-range/ l+ ## 0
//  /if-cond/   l+ ## 0
//  /if-return/ l+ ## 0
//  /if-add/    l- ## s-
//  /return/    l- ## s-
