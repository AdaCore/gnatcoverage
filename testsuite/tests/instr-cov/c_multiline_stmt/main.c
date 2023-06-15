#include "pkg.h"

int
id (int x)
{
  return x;
}

int
main ()
{
  int x = 2, i;

  // Check do-while statement. A closing brace should be inserted before the
  // while.
  do
    x++
        // dumb-token
        ;
  while (x < 2);

  // Check nested if-else statements. For each un-braced control flow stmt,
  // a closing brace should be inserted before the next statement, or before
  // the else / while if the control flow stmt is featured in a then part / a
  // do body respectively.
  if (id (x) != x)
    for (i = 0; i++; i < 10)
      return 0
          // dumb-token
          ;
  else if (foo ())
    return 0
        // dumb-token
        ;
}
