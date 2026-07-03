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
#undef DECL
#define DECL(x)                                                               \
  int x##1;                                                                   \
  int x##2;
  DECL (a_main)
#include "foo.h"
  return 0;
}
