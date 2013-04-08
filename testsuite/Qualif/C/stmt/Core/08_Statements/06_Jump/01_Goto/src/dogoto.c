#include "dogoto.h"

void
dogoto (int full)
{
  int a = 0;        // # body

  if (full != -1)   // # body
    goto test;      // # body
  a += 1;           // # unreachable

test:
  if (full)         // # body
  {
    a += 1;         // # cond
    goto end;       // # cond
  }

end:
  ;
}
