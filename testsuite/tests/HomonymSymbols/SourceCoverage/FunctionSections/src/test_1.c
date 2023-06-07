#include "sum.h"

int
main (void)
{
  int array[4] = { 1, 2, 3, 4 };
  compute_sum (array, 4, SUM_ADD);
  return 0;
}

//# sum.c
//  /sum-all/   l+ ## 0
//  /sum-add/   l+ ## 0
//  /sum-sub/   l- ## s-
//  /sum-none/  l- ## s-
