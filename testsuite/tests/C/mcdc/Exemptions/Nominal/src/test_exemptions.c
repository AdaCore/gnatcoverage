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
// /exempt_at/     l# ## x0:"whole function single line comment"
// /exempt_oe/     l* ## x+:"if stmt"
// /exempt_oe_v1/  l= ## Xc!
// /exempt_neg/    l* ## x+:"single statement, multiline comment"
// /exempt_neg_v1/ l= ## Xs-
