#include <stdbool.h>

extern void m1 (bool compute);
extern void m2 (bool compute);

void
print_log (const char *fmt, ...)
{
}

int
main ()
{
  m1 (true);
  m2 (false);
  return 0;
}

//# m1.c
//  /m1-true/	l+ ## 0
//  /m1-false/	l- ## s-
//# m2.c
//  /m2-true/	l- ## s-
//  /m2-false/	l+ ## 0
