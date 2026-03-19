#include "foo.h"
#include "helpers.h"

int
foo (int cond)
{
  if (identity (cond)) // # if-expr
    bar1 ();           // # if-then
  else
    bar2 (); // # if-else

  (identity (cond)) ? bar1 () : bar2 ();                  // # tern-void-stmt
  (identity (cond)) ? identity (cond) : identity (!cond); // # tern-int-stmt

  return ((identity (cond)) ? bar1 () : bar2 ()), 1; // # tern-void-expr
}
