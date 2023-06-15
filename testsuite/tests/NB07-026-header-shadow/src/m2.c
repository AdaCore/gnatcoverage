#include "box.h"
#include <stdbool.h>

static int v = 7;

void
m2 (bool compute)
{
  if (compute)
    print_log ("m2: twice (v) = %d\n", twice (v)); // # m2-true
  else
    print_log ("m2: don't compute\n"); // # m2-false
}
