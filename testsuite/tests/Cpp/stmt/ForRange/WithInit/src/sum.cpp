#include "sum.hh"
#include "validate.hh"

int
sum (std::vector<int> ints)
{
  int result = 0;          // # init
  for (validate (ints);    // # for-init
       const int &i : ints // # for-range
  )
    result += i; // # add
  return result; // # return
}
