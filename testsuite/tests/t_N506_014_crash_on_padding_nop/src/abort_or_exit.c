
/* All the runtimes we link against expose abort(), and we have it on native
   platforms as well. It causes abnormal termination on these platforms
   though, so we'd better use exit in this case. But exit isn't available
   everywhere ...  */

#include <abort_or_exit.h>

extern __attribute__ ((noreturn)) void abort ();
extern __attribute__ ((weak, noreturn)) void exit (int code);

__attribute__ ((noinline)) void
abort_or_exit (int code)
{
  if (exit != 0)
    exit (code);
  else
    abort ();
}
