#include "foo.h"

void
increment (int *x)
{
  (*x)++;
}

int
main ()
{
  int x = 1;
  // The only call that should not count as a violation when never executed
  // is that of the dump buffers procedure.
  if (1 == 2)
    {
      /* *******_DUMP_BUFFERS */
      return 0;
    }
  increment (&x);

  x += foo ();

  /* *******_DUMP_BUFFERS */

  return 0;
}
