#include "pkg.h"
#include "message.h"

// A forward declaration should not be instrumented for function coverage.
void bar (void); // # bar_decl

int             // # sends_42_def_1
sends_42 (void) // # sends_42_def_2
{               // # sends_42_def_2

  return 42; // # return
}

void       // # foo_def_1
foo (void) // # foo_def_2
{          // # foo_def_2

  bar (); // # bar_call

  while (sends_42 ()) // # bar_call
    break;            // # no-op

  if (sends_42 () == 42) // # bar_call
    0;                   // # no-op

  for (; sends_42 ();) // # bar_call
    break;             // # no-op
}

void       // # bar_def_1
bar (void) // # bar_def_2
{          // # bar_def_2

  // call of a function that is not in the same file or compilation unit
  print_message ("bar"); // # printf_call
}
