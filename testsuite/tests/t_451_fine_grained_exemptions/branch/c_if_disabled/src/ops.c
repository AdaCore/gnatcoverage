#include "ops.h"

void
print (const char *msg)
{
}

void
process (bool b)
{
  // GNATCOV_COV_OFF("J1") // # disabled
  if (b) // # disabled
    {    // # disabled
      // GNATCOV_COV_ON    // # disabled
      print ("b is true"); // # print_1
      print ("all good");  // # print_1
    }
  else
    // GNATCOV_EXEMPT_BRANCH("J2") // # exempt
    print ("b is false"); // # exempt_print
}
