#include "helper.h"

int
check (int argc)
{
  (argc < 10) ? (void) 0 : nop (); // # tern
  return 0;
}

int
main (void)
{
  (void) check (1);
  return 0;
}

//# test_foo.c
//  /tern/ sd=>l! ## s=>s-, d=>s-,dF-
