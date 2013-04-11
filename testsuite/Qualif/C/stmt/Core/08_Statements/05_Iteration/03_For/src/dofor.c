#include "dofor.h"

void
dofor (int start, int behavior)
{
  int a = start;                // # body

  if (behavior & GOTO_IN)       // # body
    {
      ++a;                      // # goto-in
      goto in_for;              // # goto-in
    }

  --a;                          // # pre-for

  for (a = start; a < 10; ++a)  // # eval
    {
in_for:
      if (behavior & GOTO_OUT)  // # for
        {
          ++a;                  // # goto-out
          goto out_for;         // # goto-out
        }
    }

out_for:
  ;
}
