#include <stdbool.h>

extern void print_if (bool c1, bool c2, const char *message);

int
main (void)
{
  print_if (false, false, "some message");
  print_if (true, false, "some message");
  print_if (true, true, "some message");
  return 0;
}

//# ops.c
//  /identity/  l+ ## 0
//  /condition/ l# ## 0
//  /put_line/  l+ ## 0
