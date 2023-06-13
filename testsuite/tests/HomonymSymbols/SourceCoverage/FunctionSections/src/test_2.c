#include "sum.h"

int
main (void)
{
  int array[4] = { 1, 2, 3, 4 };
  compute_sum (array, 4, SUM_SUB);
  return 0;
}

//# sum.c
//  /sum-all/   l+ ## 0
//  /sum-add/   l- ## s-
//  /sum-sub/   l+ ## 0
//  /sum-none/  l- ## s-
