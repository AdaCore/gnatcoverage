#include "validate.hh"

void
validate (std::vector<int> ints)
{
  for (const int &i : ints)
    {
      if (i == 0)
        throw "invalid null element";
    }
}
