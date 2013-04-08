#include "dofor.h"

int
main (void)
{
  dofor (0, GOTO_OUT);
  return 0;
}

//# dofor.c
//  /body/      l+ ## 0
//  /goto-in/   l- ## s-
//  /eval/      l+ ## 0
//  /for/       l+ ## 0
//  /goto-out/  l+ ## 0
