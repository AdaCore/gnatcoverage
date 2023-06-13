#include "a/pkg.h"
#include "b/pkg.h"

int
main ()
{
  volatile int dummy = return_a_number (2);
  dummy = return_some_number (2);
  return 0;
}
