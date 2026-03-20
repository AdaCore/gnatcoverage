#include "doreturn.h"

int
main (void)
{
  doreturn (0);
  return 0;
}

//# doreturn.c
//  /body/          l+ ## 0
//  /cond-true/     l- ## s-
//  /cond-false/    l+ ## 0
