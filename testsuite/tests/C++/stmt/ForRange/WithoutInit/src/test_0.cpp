#include "sum.hh"

int
main (void)
{
  volatile bool b = false;
  return b && sum (RangeIterable (1));
}

//# sum.cpp
//
//  /init/      l- ## s-
//  /for-range/ l- ## s-
//  /if-cond/   l- ## s-
//  /if-return/ l- ## s-
//  /if-add/    l- ## s-
//  /return/    l- ## s-
