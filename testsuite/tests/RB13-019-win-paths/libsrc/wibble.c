#include "wibble.h"

int
foo (int a, int b)
{
  if (a > 0 && (b == 3 || b == 7))
    {
      return 1;
    }
  else
    {
      return 0;
    }
}
