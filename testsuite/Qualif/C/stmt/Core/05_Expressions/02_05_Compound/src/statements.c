#include "statements.h"

struct complex_number
{
  int   real;
  int   imag;
};

void
run_statements (int full)
{
  (char *) {"Hello, world!"};           // # statements-all
  if (full)                             // # statements-aux-all
    (char *) {"Hello, world!"};         // # statements-cond

  (struct complex_number) { 1, 2 };     // # statements-all
  if (full)                             // # statements-aux-all
    (struct complex_number) { 1, 2 };   // # statements-cond
}
