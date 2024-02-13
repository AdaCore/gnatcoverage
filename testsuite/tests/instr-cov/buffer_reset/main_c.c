#include <stdbool.h>

extern bool and_then (bool left, bool right);

int
main (void)
{
  and_then (true, true);
  /* GNATCOV_DUMP_BUFFERS ("c-0") */
  /* GNATCOV_RESET_BUFFERS */
  and_then (true, false);
  /* GNATCOV_DUMP_BUFFERS ("c-1") */
  /* GNATCOV_RESET_BUFFERS */
  and_then (false, true);
  /* GNATCOV_DUMP_BUFFERS ("c-2") */
  return 0;
}
