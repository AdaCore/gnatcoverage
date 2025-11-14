#include "pkg.h"

// throws an error if i <= 0, returns i otherwise
static int
validate_positive (int i)
{
  if (i == 0)                               // # if_1
    throw custom_exception ();              // # throw_1
  if (i < 0)                                // # if_2
    throw std::runtime_error ("i was < 0"); // # throw_2

  return i; // # return_val
}

void
catch_not_positive (int i)
{
  try
    {
      validate_positive (i);       // # call_validate_1
      if (i + 1 == i + 1)          // # call_validate_2
        validate_positive (i + 1); // # call_validate_2
    }
  catch (const custom_exception &e)
    {
      if (i + 1 == i + 1) // # return_1
        return;           // # return_1
    }
  catch (const std::exception &e)
    {
      return; // # return_2
    }

  return; // # return_3
}
