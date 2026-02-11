#include "dofor.h"

int
dofor (int start, int behavior)
{
  int a = start; // # body
  int i;         // # body

  if (behavior & GOTO_IN) // # body
    {
      ++a;         // # goto-in
      i = start;   // # goto-in
      goto in_for; // # goto-in
    }

  --a; // # pre-for

  // For binary traces, we will have two coverage obligations here:
  //   - The initialization statement and the test expression
  //   - The update statement
  //
  // For source traces, we have an obligation for the initialization statement,
  // and another for the test expression, making it 3 (we still have the one
  // for the update statement). This means the coverage expectations will be
  // different.

  for (i = start; i < 10; ++i) // # eval
    {
    in_for:
      if (behavior & GOTO_OUT) // # for
        {
          ++a;          // # goto-out
          goto out_for; // # goto-out
        }
    }

out_for:
  return a + i; // # return
}
