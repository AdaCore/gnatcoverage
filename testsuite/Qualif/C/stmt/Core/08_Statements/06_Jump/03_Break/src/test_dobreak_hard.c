#include "dobreak.h"

int
main (void)
{
  dobreak (11, 0);
  return 0;
}

//# dobreak.c
//  /body/          l+ ## 0
//  /while/         l+ ## 0
//  /break-soft/    l- ## s-
//  /break-hard/    l+ ## 0
