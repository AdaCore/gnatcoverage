#include "doreturn.h"

int
main (void)
{
  doreturn (1);
  return 0;
}

//# doreturn.c
//  /body/          l+ ## 0
//  /cond-true/     l+ ## 0
//  /cond-false/    l- ## s-
