#include "dofor.h"

int
main (void)
{
  dofor (10, GOTO_IN);
  return 0;
}

//# dofor.c
//  /body/      l+ ## 0
//  /goto-in/   l+ ## 0
//  /pre-for/   l- ## s-
//  /eval/      l+ ## 0
//  /for/       l+ ## 0
//  /goto-out/  l- ## s-
//  /return/    l+ ## 0
