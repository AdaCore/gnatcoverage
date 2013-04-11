#include "dowhile.h"

int
main (void)
{
  dowhile (0, GOTO_IN | GOTO_OUT);
  return 0;
}

//# dowhile.c
//  /body/          l+ ## 0
//  /goto-in/       l+ ## 0
//  /eval/          l- ## s-
//  /while-eval/    l- ## s-
//  /while-in/      l+ ## 0
//  /goto-out/      l+ ## 0
