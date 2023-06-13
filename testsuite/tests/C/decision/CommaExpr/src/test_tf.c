#include "cmp.h"

int
main (void)
{
  if (!less_than (1, 2) || less_than (2, 1))
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
//  /if-cond/ l+ ## 0
//  /if-then/ l+ ## 0
//  /if-else/ l+ ## 0
