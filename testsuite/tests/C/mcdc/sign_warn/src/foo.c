#include "foo.h"

#include <stddef.h>

int
do_test (int *x, int *y, int *z)
{

  // pointer equality ops in condition
  if (x == NULL || y == NULL) // # ob
    {

      // and in decision
      if (z == NULL) // # ob
        return 0;    // # ob
      return *z;     // # ob
    }

  //  Relational ops in conditions
  if (*x < 0 && *y < 0) // # ob
    return *z;          // # ob

  // Relational ops in decision
  if (*y < 0)  // # ob
    return *y; // # ob

  return *z; // # ob
}
