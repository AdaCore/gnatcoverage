#include "dofor.h"

int
main (void)
{
  dofor (0, GOTO_IN | GOTO_OUT);
  return 0;
}

//# dofor.c
//  /body/      l+ ## 0
//  /goto-in/   l+ ## 0
//  /pre-for/   l- ## s-
//  /for/       l+ ## 0
//  /goto-out/  l+ ## 0
//  /return/    l+ ## 0

//%opts: --trace-mode=bin
//  /eval/      l- ## s-, s-
//%opts: --trace-mode=src
//  /eval/      l- ## s-, s-, s-
