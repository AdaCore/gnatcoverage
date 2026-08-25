#include "dummy.h"

int
main (void)
{
  int i;
  // GNATCOV_EXEMPT_ON ("J2")
  set_zero (&i);
  // GNATCOV_EXEMPT_OFF()
  return i;
}
