#include <stdio.h>

void
foo_c (char *s, unsigned len)
{
  printf ("Hello %.*s !\n", len, s);
}
