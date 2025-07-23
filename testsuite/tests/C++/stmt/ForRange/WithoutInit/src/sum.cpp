#include "sum.hh"

int
sum (const RangeIterable &r)
{
  int result = 0; // # init
  for (int i : r) // # for-range
    {
      if (i == 10) // # if-cond
        return 0;  // # if-return
      result += i; // # if-add
    }
  return result; // # return
}
