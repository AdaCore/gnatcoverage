#include "dowhile.h"

void
dowhile (int start, int behavior)
{
  int a = start;                // # body

  if (behavior & GOTO_IN)       // # body
    goto in_while;              // # goto-in

  while (a < 10)                // # eval
    {
in_while:
      ++a;                      // # while
      if (behavior & GOTO_OUT)  // # while
        goto out_while;         // # goto-out
    }

out_while:
  ;
}
