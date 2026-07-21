#include "ops.h"

void
print (const char *msg)
{
}

void
process (bool b)
{
  if (b) // # condition
    {    // # exempt
      // GNATCOV_EXEMPT_BRANCH("J") // # exempt
      print ("b is true"); // # exempt_print
      print ("all good");  // # exempt_print
    } // # exempt
  else
    print ("b is false"); // # print_2
}
