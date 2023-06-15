#include "fact.h"
#include "not.h"

int
main (void)
{
  compute_fact (0);
  compute_not (0);
  return 0;
}

//# fact.c
//  /fact-partial/  l+ ## 0
//  /fact-full/     l- ## s-

//# not.c
//  /not-all/       l+ ## 0
//  /not-false/     l+ ## 0
//  /not-true/      l- ## s-
