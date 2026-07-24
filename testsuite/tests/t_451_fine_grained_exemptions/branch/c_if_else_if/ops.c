#include "ops.h"

void
print (const char *msg)
{
}

void
process (bool b1, bool b2, bool b3)
{
  if (b1)
    print ("Message 1");
  else if (b2)
    {
      print ("Message 2");
    }
  else if (b3)
    print ("Message 3");
  else
    {
      print ("Message 4");
    }
}
