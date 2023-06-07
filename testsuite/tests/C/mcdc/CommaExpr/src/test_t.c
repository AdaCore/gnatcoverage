#include "cmp.h"

int
main (void)
{
  if (less_than (1, 2))
    {
      return 0;
    }
  else
    {
      return 1;
    }
}

//# cmp.c
//
//  /decl/    l+ ## 0
//  /if-cond/ l! ## dF-
//  /if-then/ l+ ## 0
//  /if-else/ l- ## s-
