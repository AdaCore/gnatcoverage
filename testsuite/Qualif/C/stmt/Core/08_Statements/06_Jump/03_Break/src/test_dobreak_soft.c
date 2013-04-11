#include "dobreak.h"

int
main (void)
{
  dobreak (5, 0);
  return 0;
}

//# dobreak.c
//  /body/          l+ ## 0
//  /while/         l+ ## 0
//  /break-soft/    l+ ## 0
//  /break-hard/    l- ## s-
