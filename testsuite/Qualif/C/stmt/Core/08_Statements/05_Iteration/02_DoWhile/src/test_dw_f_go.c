#include "dowhile.h"

int
main (void)
{
  dowhile (10, GOTO_OUT);
  return 0;
}

//# dowhile.c
//  /body/      l+ ## 0
//  /goto-in/   l- ## s-
//  /pre-while/ l+ ## 0
//  /eval/      l- ## s-
//  /while/     l+ ## 0
//  /goto-out/  l+ ## 0
