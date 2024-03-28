#include "stuff.hpp"

#include <iostream>

int
main (int argc, char **argv)
{

  (void) argc;
  (void) argv;

  Stuff a = { 5 };

  std::cout << a.data << "\n";

  /* GNATCOV_DUMP_BUFFERS */

  return 0;
}
