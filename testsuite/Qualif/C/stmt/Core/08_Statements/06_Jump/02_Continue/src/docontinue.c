#include "docontinue.h"

int
docontinue (int limit, int arg)
{
  do
    {
      ++arg;                        // # while
      if (arg > limit)              // # while
        {
          arg += 2;                 // # continue-soft
          continue;                 // # continue-soft
        }
      if (arg >= 10)                // # while
        {
          arg += 3;                 // # continue-hard
          continue;                 // # continue-hard
        }
      ++arg;                        // # not-continue
    } while (0 < arg && arg <= 10); // # eval

  return arg;                       // # body
}
