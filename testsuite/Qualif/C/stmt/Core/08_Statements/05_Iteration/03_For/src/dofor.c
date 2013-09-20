#include "dofor.h"

int
dofor (int start, int behavior)
{
  int a = start;                // # body
  int i;                        // # body

  if (behavior & GOTO_IN)       // # body
    {
      ++a;                      // # goto-in
      goto in_for;              // # goto-in
    }

  --a;                          // # pre-for

  for (i = start; i < 10; ++i)  // # eval
    {
in_for:
      if (behavior & GOTO_OUT)  // # for
        {
          ++a;                  // # goto-out
          goto out_for;         // # goto-out
        }
    }

out_for:
  return a + i;                 // # return
}
