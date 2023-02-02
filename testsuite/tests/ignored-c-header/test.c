#include "twice.h"

extern int identity (int n);

int
main ()
{
  int n = identity (twice (2));
  return 0;
}
