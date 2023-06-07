#include <abort_or_exit.h>

__attribute__ ((noinline)) __attribute__ ((noreturn)) void
stop (int code)
{
  if (code < 0)       // # stop-cond
    code = -code + 1; // # stop-then
  abort_or_exit (code);
}

int
check (int code)
{
  if (code > 0) // # check-cond
    code = 0;

  stop (code);
}

int
main (int argc, char *argv[])
{
  check (1);
}

//# test_stop.c
//  /stop-cond/   l! ## dT-
//  /stop-then/   l- ## s-
//  /check-cond/  l! ## dF-
