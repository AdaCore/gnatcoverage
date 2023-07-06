// stdlib comes with a bunch of compiler specific code, when included in a
// GNU source context. Check that its inclusion does not result in gnatcov
// instrument emitting spurious diagnostics.
#define _GNU_SOURCE 1
#include <stdlib.h>

int bar(){
  return 0;
}
