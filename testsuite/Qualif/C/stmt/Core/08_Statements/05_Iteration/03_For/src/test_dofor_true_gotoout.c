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
//  /pre-for/   l+ ## 0
//  /eval/      l! ## s-
//  /for/       l+ ## 0
//  /goto-out/  l+ ## 0
//  /return/    l+ ## 0
