#include "sum.hh"

int
main (void)
{
  if (sum (RangeIterable (-1)) != 0)
    return 1;
  return 0;
}

//# sum.cpp
//
//  /init/      l+ ## 0
//  /for-range/ l+ ## 0
//  /if-cond/   l- ## s-
//  /if-return/ l- ## s-
//  /if-add/    l- ## s-
//  /return/    l+ ## 0
