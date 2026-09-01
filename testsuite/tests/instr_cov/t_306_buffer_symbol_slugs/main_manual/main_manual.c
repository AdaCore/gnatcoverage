#include "foo.h"

int
main (void)
{
  volatile int r = foo (1);
  /* GNATCOV_DUMP_BUFFERS ("t1") */
  /* GNATCOV_RESET_BUFFERS */
  r = r + 1;
  /* GNATCOV_DUMP_BUFFERS ("t2") */
  return r == 3 ? 0 : 1;
}
