#include <stdio.h>

/* Ada code to be called.  */
extern int do_math (int l, int r);

/* Although there is no Ada runtime, we still need to provide a last chance
   handler.  */
void
__gnat_last_chance_handler (char *source_location, int line)
{
  printf ("EXCEPTION at %s\n", source_location);
}

/* putchar function provided for the coverage runtime.  */
int
gnatcov_rts_putchar (int c)
{
  return putchar (c);
}

int
main ()
{
  int y = do_math (0, 4);
  return 0;
}
