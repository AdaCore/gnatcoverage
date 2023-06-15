#include "process.h"
#include <stdio.h>
#include <stdlib.h>

void
usage ()
{
  printf ("calc <int1> <int2> <op>, print result of <int1> <op> <int2>\n");
}

int
main (int argc, const char *argv[])
{
  if (argc != 4)
    {
      usage ();
      exit (1);
    }

  process (argv);
  return 0;
}
