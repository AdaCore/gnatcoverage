#include "twice.h"

extern int id (int n);

int
main ()
{
  int n = id (twice (2));
  return 0;
}
