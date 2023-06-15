#include "cmp.h"

int
main (void)
{
  if (less_than (2, 1))
    {
      return 1;
    }
  else
    {
      return 0;
    }
}

//# cmp.c
//
//  /decl/    l+ ## 0
//  /if-cond/ l! ## dT-
//  /if-then/ l- ## s-
//  /if-else/ l+ ## 0
