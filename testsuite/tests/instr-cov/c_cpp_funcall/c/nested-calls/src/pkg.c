#include "pkg.h"

int             // # add_one_def_1
add_one (int i) // # add_one_def_2
{               // # add_one_def_2
  return i + 1; // # return
}

// Main will be called
int                     // # foo_def_1
foo (int i)             // # foo_def_2
{                       // # foo_def_2
  int x =               // # assignment
    add_one (           // # call
      add_one (         // # call
        add_one (       // # call
          add_one (     // # call
            add_one (1) // # call
            ))));       // # parentheses

  return x; // # return
}
