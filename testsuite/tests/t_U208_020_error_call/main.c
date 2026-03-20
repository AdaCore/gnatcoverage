#include <errno.h>

int
main ()
{
  errno = ENOENT;
  return 0;
}
