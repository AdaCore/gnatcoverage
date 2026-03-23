#include "foo.hpp"
#include "b.hpp"

bool
foo ()
{
  bool t = true;

  if (!t)
    {
      t = return_t () && t;
    }

  return return_t ();
}
