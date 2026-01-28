#include "pkg.h"
#include "message.h"

#define CALL_BAR bar ()

void                     // # bar_def_1
bar (void)               // # bar_def_2
{                        // # bar_def_2
  print_message ("bar"); // # printf_call
}

void       // # foo_def_1
foo (void) // # foo_def_2
{          // # foo_def_2

  CALL_BAR; // # bar_call
}
