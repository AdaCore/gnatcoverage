#include <stdbool.h>
#include <stdio.h>

int
main ()
{
  bool all_good = true;
  bool all_good_really = true;
  printf ("Hello ");
  if (all_good && all_good_really)
    printf ("world!\n");
  else
    printf ("... actually no");
  return 0;
}
