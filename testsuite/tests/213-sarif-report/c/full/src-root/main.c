#include <stdbool.h>

#include "bar.h"
#include "foo.h"
#include "lib.h"

int
main (void)
{
  int t = bar ();
  int f = false;

  if (lib_identity (t) || f)
    {
      t = t && true;
    }

  if (f && foo ())
    {
      f = f && false;
    }

  /* GNATCOV_EXEMPT_ON "justification" */
  if (!foo ())
    {
      t = !f;
    }
  /* GNATCOV_EXEMPT_OFF */

  return 0;
}
