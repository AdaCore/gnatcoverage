#include "statements.h"

int
run_statements (int full, int arg)
{
  arg   = arg + 1;      // # statements-all
  arg  *= 2;            // # statements-all
  arg  /= 1;            // # statements-all
  arg  %= 0xff;         // # statements-all
  arg  += 1;            // # statements-all
  arg  -= 1;            // # statements-all
  arg <<= 1;            // # statements-all
  arg >>= 1;            // # statements-all
  arg  &= 0xff;         // # statements-all
  arg  ^= 1;            // # statements-all
  arg  |= 1;            // # statements-all
  if (full)             // # statements-aux-all
    {
      arg   = arg + 1;  // # statements-cond
      arg  *= 2;        // # statements-cond
      arg  /= 1;        // # statements-cond
      arg  %= 0xff;     // # statements-cond
      arg  += 1;        // # statements-cond
      arg  -= 1;        // # statements-cond
      arg <<= 1;        // # statements-cond
      arg >>= 1;        // # statements-cond
      arg  &= 0xff;     // # statements-cond
      arg  ^= 1;        // # statements-cond
      arg  |= 1;        // # statements-cond
    }

  return arg;       // # statements-aux-all
}
