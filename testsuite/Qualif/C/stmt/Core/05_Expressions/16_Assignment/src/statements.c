#include "statements.h"

void
run_statements (int full)
{
  int a = 0;        // # statements-aux-all

  a   = 0;          // # statements-all
  a  *= 2;          // # statements-all
  a  /= 1;          // # statements-all
  a  %= 2;          // # statements-all
  a  += 1;          // # statements-all
  a  -= 1;          // # statements-all
  a >>= 1;          // # statements-all
  a <<= 1;          // # statements-all
  a  &= 1;          // # statements-all
  a  ^= 1;          // # statements-all
  a  |= 1;          // # statements-all
  if (full)         // # statements-aux-all
    {
      a   = 0;      // # statements-cond
      a  *= 2;      // # statements-cond
      a  /= 1;      // # statements-cond
      a  %= 2;      // # statements-cond
      a  += 1;      // # statements-cond
      a  -= 1;      // # statements-cond
      a >>= 1;      // # statements-cond
      a <<= 1;      // # statements-cond
      a  &= 1;      // # statements-cond
      a  ^= 1;      // # statements-cond
      a  |= 1;      // # statements-cond
    }
}
