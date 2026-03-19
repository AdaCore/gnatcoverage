#include <stdio.h>

void
write_error (const char *)
{
}

// Dummy implementation of assert that expands to multiple statements.
#define assert(expr)                                                          \
  ({                                                                          \
    int _result = expr;                                                       \
    if (!_result)                                                             \
      write_error ("assertion failure: " #expr);                              \
  })

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
//	Given the current state of the technology, one of the expanded
//	statements is expected not to be covered.  See NB17-025.
