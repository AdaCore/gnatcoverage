#ifndef OPS_H
#define OPS_H

#include <stdbool.h>

extern void put_line (const char *message);
extern bool bool_id (bool b);

static void
print_if (bool c1, bool c2, const char *message)
{
  /* GNATCOV_EXEMPT_DECISION_OUTCOME(false, 1, "never false") */
  if (c1 && bool_id (c2 ? bool_id (true) : bool_id (false))) // # condition
    {
      put_line (message); // # put_line
    }
}

#endif
