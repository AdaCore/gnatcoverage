#include "foo.h"

int *
bar_false (void)
{
  get_process (1);            // # bar-false
  set_current (identity (0)); // # bar-false
  return process_current ();  // # bar-false
}
