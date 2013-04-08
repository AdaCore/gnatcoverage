#include "dofor.h"

int
main (void)
{
  dofor (10, 0);
  return 0;
}

//# dofor.c
//  /body/      l+ ## 0
//  /goto-in/   l- ## s-
//  /eval/      l+ ## 0
//  /for/       l- ## s-
//  /goto-out/  l- ## s-
