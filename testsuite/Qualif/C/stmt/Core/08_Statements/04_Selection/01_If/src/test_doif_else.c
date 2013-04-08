#include "doif.h"

int
main (void)
{
  int xst;
  doif (0, &xst);
  return 0;
}

//# doif.c
//  /body/  l+ ## 0
//  /eval/  l+ ## 0
//  /if/    l- ## s-
//  /else/  l+ ## 0
