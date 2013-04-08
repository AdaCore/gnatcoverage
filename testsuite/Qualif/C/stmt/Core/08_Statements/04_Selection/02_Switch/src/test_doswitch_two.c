#include "doswitch.h"

int
main (void)
{
  int xst;
  doswitch (2, &xst);
  return 0;
}

//# doswitch.c
//  /body/      l+ ## 0
//  /eval/      l+ ## 0
//  /zero/      l- ## s-
//  /one/       l- ## s-
//  /two/       l+ ## 0
//  /default/   l- ## s-
