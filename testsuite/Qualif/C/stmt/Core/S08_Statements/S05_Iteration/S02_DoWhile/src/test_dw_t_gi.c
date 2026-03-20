#include "dowhile.h"

int
main (void)
{
  dowhile (0, GOTO_IN);
  return 0;
}

//# dowhile.c
//  /body/      l+ ## 0
//  /goto-in/   l+ ## 0
//  /pre-while/ l- ## s-
//  /eval/      l+ ## 0
//  /while/     l+ ## 0
//  /goto-out/  l- ## s-
