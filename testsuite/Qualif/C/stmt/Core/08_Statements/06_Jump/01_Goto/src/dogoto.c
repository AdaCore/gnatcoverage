#include "dogoto.h"

int
dogoto (int full, int arg)
{
  if (full != -1)   // # body
    {
      --arg;        // # body
      goto test;    // # body
    }
  ++arg;            // # unreachable

test:
  if (full)         // # body
  {
    ++arg;          // # cond
    goto end;       // # cond
  }

end:
  return arg;       // # body
}
