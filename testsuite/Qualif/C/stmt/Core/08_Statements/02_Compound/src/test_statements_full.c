#include "statements.h"

int
main (void)
{
  run_statements (1, 0);
  return 0;
}

//# statements.c
//  /statements-all/        l+ ## 0
//  /statements-aux-all/    l+ ## 0
//  /statements-cond/       l+ ## 0
//  /statements-aux-cond/   l+ ## 0
//  /statements-not-cond/   l- ## s-

//%opts: --trace-mode=src
//  /statement-non-coverable/      l- ## s-
