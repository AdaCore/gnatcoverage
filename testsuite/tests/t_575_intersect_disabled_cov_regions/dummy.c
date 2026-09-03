#include "dummy_0.h"
#include "dummy_1.h"

int
main (void)
{
  int i;
  // GNATCOV_COV_OFF ("JM")
  set_one (&i);
  set_zero (&i);
  // GNATCOV_COV_ON()
  return i;
}
