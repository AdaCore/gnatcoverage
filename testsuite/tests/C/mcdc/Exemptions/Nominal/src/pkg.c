#include "pkg.h"
#include <stdbool.h>

#define NOT(A) !A

// GNATCOV_EXEMPT_ON "whole function single line comment" // # exempt_at
bool                     // # exempt_at
andThen (bool a, bool b) // # exempt_at
{                        // # exempt_at
  return a && b;         // # exempt_at
} // # exempt_at
// GNATCOV_EXEMPT_OFF                                     // # exempt_at

bool
orElse (bool a, bool b)
{
  // GNATCOV_EXEMPT_ON "if stmt"  // # exempt_oe
  if (a || b) // # exempt_oe_v1
    {         // # exempt_oe
      // GNATCOV_EXEMPT_OFF       // # exempt_oe
      return true;
    }
  else
    {
      return false;
    }
}

bool
negation (bool a)
{
  /* GNATCOV_EXEMPT_ON "single statement, multiline comment"*/ // # exempt_neg
  return NOT (a);          // # exempt_neg_v1
  /* GNATCOV_EXEMPT_OFF */ // # exempt_neg
}
