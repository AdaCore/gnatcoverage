#include "dowhile.h"

int
dowhile (int start, int behavior)
{
  int a = start;                // # body

  if (behavior & GOTO_IN)       // # body
    {
      ++a;                      // # goto-in
      goto in_while;            // # goto-in
    }

  /* Without the following statement, the goto-in is pointless.  */
  ++a;                          // # pre-while

    do {
in_while:
      ++a;                      // # while
      if (behavior & GOTO_OUT)  // # while
        {
          ++a;                  // # goto-out
          goto out_while;       // # goto-out
        }
    } while (a < 10);           // # eval

out_while:
  return a;                     // # body
}
