#include "a/pkg.h"
#include "b/pkg.h"

int
main ()
{
  volatile int dummy = return_a_number (-3);
  dummy = return_some_number (-1);
  return 0;
}
