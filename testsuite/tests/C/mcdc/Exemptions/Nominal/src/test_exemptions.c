#include "pkg.h"

int
main (void)
{
  volatile bool x = orElse (false, true);
  x = orElse (false, false);
  x = andThen (true, true);
  x = andThen (true, false);
  x = andThen (false, false);
  return 0;
}

//# pkg.c
//
// /exempt_at/     l# ## x0
// /exempt_oe/     l* ## x+
// /exempt_oe_v1/  l= ## Xc!
// /exempt_neg/    l* ## x+
// /exempt_neg_v1/ l= ## Xs-
