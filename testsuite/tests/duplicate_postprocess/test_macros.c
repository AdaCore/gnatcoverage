#include <stdio.h>

#define PRINT_HW printf ("Hello world!\n");

// Code that is not covered to have coverage violations for code inside macro
// expansions.
void
macro_stmts ()
{
  PRINT_HW;
  PRINT_HW;
}

int
main ()
{
  return 0;
}
