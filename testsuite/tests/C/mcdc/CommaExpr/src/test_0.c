#include "cmp.h"

int
main (void)
{
  volatile int b = 0;
  return b && less_than (1, 2);
}

//# cmp.c
//
//  /decl/    l- ## s-
//  /if-cond/ l- ## s-
//  /if-then/ l- ## s-
//  /if-else/ l- ## s-
