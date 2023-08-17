#define DISABLE_WARNINGS                                        \
  _Pragma ("GCC diagnostic push")                               \
  _Pragma ("GCC diagnostic ignored \"-Wimplicit-fallthrough\"")

DISABLE_WARNINGS

int
main ()
{
  return 0; // # return
}

//# test_pragma.c
//
//  /return/ l+ ## 0
