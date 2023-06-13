#include "process.h"
#include <assert.h>
#include <stdio.h>

void
process (const char *argv[])
{
  int x = atoi (argv[1]), y = atoi (argv[2]);
  char opcode = argv[3][0];

  int result;

  switch (opcode)
    {
    case '*':
      result = x * y; // # mult
      break;          // # mult
    case '+':
      result = x + y; // # plus
      break;          // # plus
    default:
      printf ("unsupported opcode %c\n", opcode); // # unsupp
      return;                                     // # unsupp
    }

  printf ("%d %c %d = %d\n", x, opcode, y, result); // # result
}
