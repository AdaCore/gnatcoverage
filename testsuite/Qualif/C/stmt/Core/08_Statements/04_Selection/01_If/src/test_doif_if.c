#include "doif.h"

int
main (void)
{
  int xst;
  doif (1, &xst);
  return 0;
}

//# doif.c
//  /body/  l+ ## 0
//  /eval/  l+ ## 0
//  /if/    l+ ## 0
//  /else/  l- ## s-
