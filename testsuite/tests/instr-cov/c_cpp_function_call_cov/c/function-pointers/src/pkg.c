#include "pkg.h"
#include <stdio.h>

int             // # add_one_def_1
add_one (int i) // # add_one_def_2
{               // # add_one_def_2
  return i + 1; // # return_one
}

int             // # add_two_def_1
add_two (int i) // # add_two_def_2
{               // # add_two_def_2
  return i + 2; // # return_two
}

void                             // # print_result_def_1
print_result (int i, fun_type f) // # print_result_def_2
{                                // # print_result_def_2
  printf (                       // # print_call
    "Result: %i",                // # print_format
    f (i)                        // # fpointer_use
  );                             // # parenthesis
}

void             // # foo_def_1
foo (fun_type f) // # foo_def_2
{                // # foo_def_2

  print_result (1, // # print_result_call
                f  // # fpointer_ref
  );               // # parenthesis
}
