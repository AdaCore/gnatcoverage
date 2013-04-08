#include "docontinue.h"

void docontinue (int limit)
{
  int a = 0;                    // # body

  do
    {
      ++a;                      // # while
      if (a > limit)            // # while
        {
          a += 2;               // # continue-soft
          continue;             // # continue-soft
        }
      if (a >= 10)              // # while
        {
          a += 3;               // # continue-hard
          continue;             // # continue-hard
        }
      ++a;                      // # not-continue
    } while (0 < a && a <= 10); // # eval
}
