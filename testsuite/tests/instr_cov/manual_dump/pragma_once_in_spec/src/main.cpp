#include "stuff.hpp"

void
print_int (int)
{
}

int
main (int argc, char **argv)
{

  (void) argc;
  (void) argv;

  Stuff a = { 5 };

  print_int (a.data);

  /* GNATCOV_DUMP_BUFFERS */

  return 0;
}
