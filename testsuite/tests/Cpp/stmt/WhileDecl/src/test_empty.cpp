#include "count_digits.hh"

int
main (void)
{
  if (count_digits ("") != 0)
    return 1;
  return 0;
}

//# count_digits.cpp
//
//  /init/       l+ ## 0
//  /while-cond/ l+ ## 0
//  /if-cond/    l- ## s-
//  /if-incr/    l- ## s-
//  /return/     l+ ## 0
