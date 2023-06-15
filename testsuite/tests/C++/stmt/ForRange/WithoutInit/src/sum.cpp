#include "sum.hh"

int
sum (std::vector<int> ints)
{
  int result = 0;           // # init
  for (const int &i : ints) // # for-range
    {
      if (i == 0)  // # if-cond
        return 0;  // # if-return
      result += i; // # if-add
    }
  return result; // # return
}
