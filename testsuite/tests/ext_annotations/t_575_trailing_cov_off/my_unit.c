#include "my_header_1.h"
#include "my_header_2.h"
#include "my_header_3.h"

int
main (void)
{
  int a = 0;
  my_f1 (&a);
  my_f2 (&a);
  my_f3 (&a);
  return 0;
}
