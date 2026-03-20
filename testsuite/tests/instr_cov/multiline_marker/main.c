#include <assert.h>

int
main (int argc, char *argv[])
{
  volatile int a = 0, b = 0;

  // Implicit example: the expansion of the assert macro will generate
  // redundant line markers.
  assert (a == 0 && b == 0);

  // Explicit example with redundant line markers
#line 14
  int c;
#line 14
  return 0;
}
