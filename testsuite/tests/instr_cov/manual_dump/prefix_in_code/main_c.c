#include <stdio.h>

extern const char *get_prefix_c (void);

int
main (void)
{
  printf ("Hello C world!\n");
  /* GNATCOV_DUMP_BUFFERS (get_prefix_c ()) */
  return 0;
}
