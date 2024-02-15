#include <stdio.h>

int
main (void)
{
  char *trace_prefix = "c_trace";
  printf ("Hello C world!\n");
  /* GNATCOV_DUMP_BUFFERS (trace_prefix) */
  return 0;
}
