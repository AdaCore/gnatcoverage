#include "dofor.h"

void
dofor (int start, int behavior)
{
  int a;                        // # body

  if (behavior & GOTO_IN)       // # body
    goto in_for;                // # goto-in

  for (a = 0; a < 10; ++a)      // # eval
    {
in_for:
      if (behavior & GOTO_OUT)  // # for
        goto out_for;           // # goto-out
    }

out_for:
  ;
}
