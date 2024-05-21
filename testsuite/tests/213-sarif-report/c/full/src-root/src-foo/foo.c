#include "foo.h"
#include "b.h"

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
