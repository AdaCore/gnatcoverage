#include "docontinue.h"

int
main (void)
{
  docontinue (5, 0);
  return 0;
}

//# docontinue.c
//  /body/          l+ ## 0
//  /while/         l+ ## 0
//  /continue-soft/ l+ ## 0
//  /continue-hard/ l- ## s-
//  /not-continue/  l+ ## 0
//  /eval/          l+ ## 0
