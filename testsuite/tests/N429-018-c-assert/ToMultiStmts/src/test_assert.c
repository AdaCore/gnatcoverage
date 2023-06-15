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
//  /stmt-assert/ l! ## s-
//	On GNU/Linux, "assert" is usually implemented as a macro which, after
//	expansion, yields multiple statements.  Given the current state of the
//	technology, one of them is expected not to be covered.  See NB17-025.
