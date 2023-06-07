#include "wibble.h"

#include <stdint.h>
#include <stdio.h>

int
check (int condition)
{
  if (condition)
    {
      printf ("PASS\n");
    }
  else
    {
      printf ("FAIL\n");
    }
}

int
main (int argc, char **argv)
{
  check (foo (3, 2) == 0);
  check (foo (3, 3) == 1);
  // check(foo(3, 7) == 1);
  check (foo (0, 0) == 0);
}
