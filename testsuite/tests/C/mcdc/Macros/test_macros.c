#include "pkg.h"
#include <stdio.h>

#define PRINT_HW printf ("Hello world!\n");
#define PRINT printf
#define HW "Hello world!\n"
#define PRINT_HW_PRINT_HW                                                     \
  PRINT_HW;                                                                   \
  PRINT_HW
#define WRAPPER_PRINT_HW PRINT_HW
#define ID(x) x
#define AND(x, y) ID (x) && y
#define WRAP_AND(x, y) AND (x, y)
#define WRAP_CMDLINE_MACRO_STMT CMDLINE_MACRO_STMT

void
macro_stmts ()
{
  PRINT_HW_PKG;
  PRINT_HW;
  WRAPPER_PRINT_HW;
  PRINT ("Hello world!\n");
  PRINT (HW);
  PRINT_HW_PRINT_HW;
  WRAP_CMDLINE_MACRO_STMT;
  CMDLINE_MACRO_STMT;
  CMDLINE_MACRO_DECL (a);
  CMDLINE_MACRO_NO_VALUE (int b =);
  // Test that coverage obligations coming from built-in macros are reported
  // correctly.
  __linux + 1;
}

void
macro_decision_violations (int a, int b)
{
  if (WRAP_AND (ID (a), b))
    PRINT_HW;
}

int
main ()
{
  macro_decision_violations (1, 0);
  macro_decision_violations (1, 1);
  return 0;
}
