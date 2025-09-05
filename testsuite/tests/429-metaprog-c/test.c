#define DECL(x) int x = 0;

int
foo ()
{
  DECL (a_main)
#include "foo.h"
}

int
main ()
{
  return 0;
}
