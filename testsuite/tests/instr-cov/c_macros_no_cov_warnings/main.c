#include <stdio.h>

extern void print_str (const char *str);

#define PRINT_HW print_str ("Hello world!\n");

int
main (void)
{
  PRINT_HW;
  return 0;
}
