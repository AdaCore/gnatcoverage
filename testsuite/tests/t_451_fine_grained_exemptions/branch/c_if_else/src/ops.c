#include "ops.h"

void
print (const char *msg)
{
}

void
process (bool b)
{
  if (b) // # condition
    {
      print ("b is true"); // # print_1
      print ("all good");  // # print_1
    }
  else
    // GNATCOV_EXEMPT_BRANCH("J") // # exempt
    print ("b is false"); // # exempt_print
}
