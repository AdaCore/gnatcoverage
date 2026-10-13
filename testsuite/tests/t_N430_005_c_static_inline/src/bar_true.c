#include "foo.h"

int *
bar_true (void)
{
  get_process (1);            // # bar-true
  set_current (identity (1)); // # bar-true
  return process_current ();  // # bar-true
}
