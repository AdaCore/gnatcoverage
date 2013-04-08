#include "dobreak.h"

void
dobreak (int limit)
{
  int a = 0;            // # body
  int loop = 1;         // # body

  do
    {
      ++a;              // # while
      if (a > limit)    // # while
        break;          // # break-soft
      if (a > 10)       // # while
        break;          // # break-hard
    } while (loop);     // # body
}
