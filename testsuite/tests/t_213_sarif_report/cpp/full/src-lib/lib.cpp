#include "lib.hpp"

bool
lib_identity (bool b)
{
  if (!b && b)
    {
      return b;
    }

  return b;
}
