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
//  /eval/      l+ ## 0
//  /while/     l- ## s-
//  /goto-out/  l- ## s-
