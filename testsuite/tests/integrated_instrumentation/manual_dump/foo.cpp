#include "foo.hh"

#include <iostream>
#include <ostream>

void
foo_dump (void)
{
  /* GNATCOV_DUMP_BUFFERS */
}

void
foo_warn (const char *s)
{
  std::cout << "WARN :" << s << std::endl;
}

void
foo_error (const char *s)
{
  std::cout << "ERROR:" << s << std::endl;
}
