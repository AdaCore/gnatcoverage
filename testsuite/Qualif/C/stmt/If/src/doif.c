
#include "doif.h"

void doif (int cond, int * xstatus)
{
  *xstatus = 0;           // # body
  if (cond)               // # eval
    *xstatus |= XST_IF;   // # if
  else
    *xstatus |= XST_ELSE; // # else
}

