#include <cassert>

#include "foo.hh"

int
main ()
{
  foo_warn ("Hello there");
  foo_dump ();
  foo_error ("General Kenobi");
}
