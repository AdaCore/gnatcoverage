#include <stdbool.h>

extern bool and_then (bool left, bool right);

int
main (void)
{
  and_then (true, true);
  /* *******_DUMP_BUFFERS ("c-0") */
  /* *******_RESET_BUFFERS */
  and_then (true, false);
  /* *******_DUMP_BUFFERS ("c-1") */
  /* *******_RESET_BUFFERS */
  and_then (false, true);
  /* *******_DUMP_BUFFERS ("c-2") */
  return 0;
}
