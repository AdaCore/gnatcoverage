#include <assert.h>

int
main (int argc, char *argv[])
{
  volatile int a = 0; // # stmt-simple
  assert (a == 0);    // # stmt-assert
  return 0;           // # stmt-simple
}

//# test_assert.c
//  /stmt-simple/ l+ ## 0
//  /stmt-assert/ l+ ## 0
//	On Windows, "assert" is usually implemented as a macro which, after
//	expansion, yields a single statement.
