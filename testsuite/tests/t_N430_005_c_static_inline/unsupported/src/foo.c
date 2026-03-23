int
one_int (void)
{
#define TYPE int
#include "foo.h"
#undef TYPE
}

unsigned
one_unsigned (void)
{
#define TYPE unsigned
#include "foo.h"
#undef TYPE
}
