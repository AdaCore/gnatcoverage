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

  while (a < 10)                // # eval
    {
      ++a;                      // # while-eval
in_while:
      ++a;                      // # while-in
      if (behavior & GOTO_OUT)  // # while-in
        {
          ++a;                  // # goto-out
          goto out_while;       // # goto-out
        }
    }

out_while:
  return a;                     // # body
}
