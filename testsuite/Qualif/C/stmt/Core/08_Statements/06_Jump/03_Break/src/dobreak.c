#include "dobreak.h"

int
dobreak (int limit, int arg)
{
  int loop = 1;         // # body

  do
    {
      ++arg;            // # while
      if (arg > limit)  // # while
        {
          --arg;        // # break-soft
          break;        // # break-soft
        }
      if (arg > 10)     // # while
        {
          --arg;        // # break-hard
          break;        // # break-hard
        }
    } while (++loop);   // # body

  return arg;           // # body
}
