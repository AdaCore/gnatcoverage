#include "docontinue.h"

int
main (void)
{
  docontinue (11);
  return 0;
}

//# docontinue.c
//  /body/          l+ ## 0
//  /while/         l+ ## 0
//  /continue-soft/ l- ## s-
//  /continue-hard/ l+ ## 0
//  /not-continue/  l+ ## 0
//  /eval/          l+ ## 0
