#include "foo.h"

#include <stddef.h>

int
main ()
{
  // Aiming for full coverage
  int x, y, z;

  x = 1;
  y = 2;
  z = 3;
  do_test (NULL, NULL, NULL);
  do_test (NULL, NULL, &z);
  do_test (&x, NULL, NULL);
  do_test (&x, &y, &z);

  x = -2;
  y = 1;
  z = 3;
  do_test (&x, &y, &z);

  x = 2;
  y = -1;
  z = 3;
  do_test (&x, &y, &z);

  x = -2;
  y = -1;
  z = 3;
  do_test (&x, &y, &z);
}

//# foo.c
//
// /ob/ l+ ## 0
