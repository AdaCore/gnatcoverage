#include "doswitch.h"

void
doswitch (int input, int * xstatus)
{
  *xstatus = 0;                         // # body

  if (input == 0)                       // # body
    {
      *xstatus &= ~XST_INPUT_WAS_ZERO;  // # zero
      goto zero;                        // # zero
    }

  switch (input)                        // # eval
    {
zero:
      *xstatus |= XST_ZERO;             // # zero
      break;                            // # zero

    case 1:
      *xstatus |= XST_ONE;              // # one

    case 2:
      *xstatus |= XST_TWO;              // # two
      break;                            // # two

    default:
      *xstatus |= XST_DEFAULT;          // # default
      break;                            // # default
    }
}
