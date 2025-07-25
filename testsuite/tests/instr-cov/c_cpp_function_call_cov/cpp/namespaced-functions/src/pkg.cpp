#include "pkg.h"

namespace sub_module
{
void hello_world (void);

void                   // # hello_def_1
hello_world (void)     // # hello_def_2
{                      // # hello_def_2
  print_msg ("Hello"); // # print
}
}

void       // # foo_def_1
foo (void) // # foo_def_2
{          // # foo_def_2

  sub_module::      // # submodule
    hello_world (); // # call
}

void       // # bar_def_1
bar (void) // # bar_def_2
{          // # bar_def_2

  using namespace sub_module; // # using_clause

  hello_world (); // # both
}
