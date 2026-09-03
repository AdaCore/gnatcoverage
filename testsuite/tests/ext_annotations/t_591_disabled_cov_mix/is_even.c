#include "is_even.h"

int
is_even (int i)
{
  /* Placeholder for external annotations.  */

  /* GNATCOV_COV_OFF("test") */ /* REMOVEME */
  /* GNATCOV_COV_ON() */        /* REMOVEME */

  if (modulo_2 (i % 2) == 0)
    return 1;

  else
    return 0;
}
