#include "pkg.h"
#include <stdbool.h>

#define NOT(A) !A

// ***************** "whole function single line comment" // # exempt_at
bool                     // # exempt_at
andThen (bool a, bool b) // # exempt_at
{                        // # exempt_at
  return a && b;         // # exempt_at
} // # exempt_at
// ******************                                     // # exempt_at

bool
orElse (bool a, bool b)
{
  // ***************** "if stmt"  // # exempt_oe
  if (a || b) // # exempt_oe_v1
    {         // # exempt_oe
      // ******************       // # exempt_oe
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
  /* ***************** "single statement, multiline comment"*/ // # exempt_neg
  return NOT (a);          // # exempt_neg_v1
  /* ****************** */ // # exempt_neg
}
