#include "a/foo.h"
#include "b/foo.h"

int
main (void)
{
  volatile int a = 1;          // # dummy
  a = foo (a) * other_foo (a); // # dummy

  return 0; // # dummy
}

//# test_foo.c
//  /dummy/        l+ ## 0

//# +a/foo.h
//  /toplev-stmt/   l+ ## 0
//  /toplev-true/   l+ ## 0
//  /toplev-false/  l- ## s-

//# +b/foo.h
//  /other-stmt/    l+ ## 0
//  /other-true/    l+ ## 0
//  /other-false/   l- ## s-
