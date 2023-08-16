#include <initializer_list>

int
main (void)
{
  int sum = 0;               // # init
  for (auto i : { 1, 2, 3 }) // # for-range
    {
      sum += i;              // # for-body
    }
  return 0;
}

//# test_for_range.cpp
//
//  /init/      l+ ## 0
//  /for-range/ l+ ## 0
//  /for-body/  l+ ## 0
