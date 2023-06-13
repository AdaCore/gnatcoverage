#include "shared.hpp"

extern void foo ();

extern "C"
{
  extern void bar ();
}

int
main ()
{
  shared ();
  foo ();
  bar ();
}
