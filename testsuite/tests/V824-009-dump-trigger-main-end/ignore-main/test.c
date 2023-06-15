#include "foo.h"

int
main ()
{
  int volatile a = 1, b = 0;
  if (a && b)
    return 0;
  else
    return foo ();
}
