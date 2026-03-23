#include "wibble.h"

#include <stdint.h>
#include <stdio.h>

extern void utils_print (const char *msg);

int
check (int condition)
{
  if (condition)
    {
      utils_print ("PASS\n");
    }
  else
    {
      utils_print ("FAIL\n");
    }
}

int
driver_main (void)
{
  check (foo (3, 2) == 0);
  check (foo (3, 3) == 1);
  // check(foo(3, 7) == 1);
  check (foo (0, 0) == 0);
}
