#include <stdio.h>

extern void cpp_func (void);

void
c_func (void)
{
  puts ("c:c_func");
  cpp_func ();
}
