#include "count_digits.hh"

int
main (void)
{
  if (count_digits ("aa00") != 2)
    return 1;
  return 0;
}

//# count_digits.cpp
//
//  /init/       l+ ## 0
//  /while-cond/ l+ ## 0
//  /if-cond/    l+ ## 0
//  /if-incr/    l+ ## 0
//  /return/     l+ ## 0
