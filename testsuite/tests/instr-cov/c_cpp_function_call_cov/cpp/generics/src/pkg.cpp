#include "pkg.h"

template <class T> void print_point (T p);

template <class T>
void              // # print_point_def_1
print_point (T p) // # print_point_def_2
{                 // # print_point_def_2
  (void) p;       // # printing
}

void       // # foo_def_1
foo (void) // # foo_def_2
{          // # foo_def_2

  int A = 13; // # var_decl

  print_point (A); // # print_call
}
