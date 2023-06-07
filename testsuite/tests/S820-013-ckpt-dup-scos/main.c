#include "bar.h"

extern int foo (int);

int
main (void)
{
  int n = fact (3);
  foo (fact (3));
  return 0;
}
