#include "not.h"
#include "support.h"

/* -O1 removes this "f" function if it is static.  */
int
f (int b)
{
  /* -O1 inlines the call to "f" from "compute_not" without the following call
     to "identity".  Inlining yields unexpected results in object coverage.  */
  if (!identity (b)) // # not-all
    return 1;        // # not-false
  else
    return 0; // # not-true
}

int
compute_not (int b)
{
  return f (b); // # not-all
}
