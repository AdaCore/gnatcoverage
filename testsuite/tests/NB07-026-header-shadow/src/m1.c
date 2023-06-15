#include "box.h"
#include <stdbool.h>

static int v = 12;

void
m1 (bool compute)
{
  if (compute)
    print_log ("m1: twice (v) = %d\n", twice (v)); // # m1-true
  else
    print_log ("m1: don't compute\n"); // # m1-false
}
