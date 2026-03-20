#include "doswitch.h"

#define NULL 0

int
main (void)
{
  int xst;
  doswitch (0, &xst);
  return 0;
}

//# doswitch.c
//  /body/      l+ ## 0
//  /eval/      l- ## s-
//  /zero/      l+ ## 0
//  /one/       l- ## s-
//  /two/       l- ## s-
//  /default/   l- ## s-
