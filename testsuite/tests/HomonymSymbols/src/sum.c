#include "sum.h"

int
sum_add (int *array, unsigned size)
{
  int result = 0; // # sum-add
  unsigned i = 0; // # sum-add

  while (i < size) // # sum-add
    {
      result += array[i]; // # sum-add
      ++i;                // # sum-add
    }

  return result; // # sum-add
}

int
sum_sub (int *array, unsigned size)
{
  int result = 0; // # sum-sub
  unsigned i = 0; // # sum-sub

  while (i < size) // # sum-sub
    {
      result -= array[i]; // # sum-sub
      ++i;                // # sum-sub
    }

  return result; // # sum-sub
}

int
compute_sum (int *array, unsigned size, int mode)
{
  if (mode == SUM_ADD)            // # sum-all
    return sum_add (array, size); // # sum-add
  else if (mode == SUM_SUB)       // # sum-sub
    return sum_sub (array, size); // # sum-sub
  else
    return 0; // # sum-none
}
