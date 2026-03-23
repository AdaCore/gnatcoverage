#include "static_lib.h"
#include <stdlib.h>

int
main (void)
{
  int a = bad_is_even (4);
  if (a)
    abort ();
  return 0;
}
