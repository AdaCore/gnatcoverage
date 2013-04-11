#include "statements.h"

struct complex_number
{
  int   real;
  int   imag;
};

int
run_statements (int full, int arg)
{
  (char *) {"Hello, world!"};           // # statements-all
  if (full)                             // # statements-aux-all
    {
      (char *) {"Hello, world!"};       // # statements-cond
      ++arg;                            // # statements-aux-cond
      /* The following statement is needed so that the previous statement is
         considered as covered even if it generates no code.  */
    }

  (struct complex_number) { 1, 2 };     // # statements-all
  if (full)                             // # statements-aux-all
    {
      (struct complex_number) { 1, 2 }; // # statements-cond
      ++arg;                            // # statements-aux-cond
      /* The following statement is needed so that the previous statement is
         considered as covered even if it generates no code.  */
    }

  return arg;                           // # statements-aux-all
}
