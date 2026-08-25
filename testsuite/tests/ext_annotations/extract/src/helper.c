#include "helper.h"

int
classify (int x)
{
  /* GNATCOV_EXEMPT_ON ("unreachable in practice") */
  if (x < 0)
    return -1;
  /* GNATCOV_EXEMPT_OFF */
  return x > 10 ? 1 : 0;
}
