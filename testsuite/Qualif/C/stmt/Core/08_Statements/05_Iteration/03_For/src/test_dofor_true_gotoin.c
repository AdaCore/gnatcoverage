#include "dofor.h"

int
main (void)
{
  dofor (0, GOTO_IN);
  return 0;
}

//# dofor.c
//  /body/      l+ ## 0
//  /goto-in/   l+ ## 0
//  /pre-for/   l- ## s-
//  /for/       l+ ## 0
//  /goto-out/  l- ## s-
//  /return/    l+ ## 0

//%opts: --trace-mode=bin
//  /eval/      l+ ## 0
//%opts: --trace-mode=src
//  /eval/      l! ## s-
