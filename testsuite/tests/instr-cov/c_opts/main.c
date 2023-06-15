#include "hello.h"

int
main (void)
{
#ifdef A
  hello ("A");
#endif

#ifdef B
  hello ("B");
#endif

  return 0;
}
