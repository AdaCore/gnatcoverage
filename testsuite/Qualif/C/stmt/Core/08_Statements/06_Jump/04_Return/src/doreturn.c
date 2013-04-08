#include "doreturn.h"

int
doreturn (int cond)
{
  if (cond)     // # body
    return 1;   // # cond-true
  else
    return 0;   // # cond-false
}
