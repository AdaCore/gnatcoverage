#include "pkg.h"

int
main (void)
{
  return 0;
}

//# pkg.cpp
// /foo_def/        l- ## f-
// /lambda_assign/  l- ## s-
// /lambda_def/     l- ## f-
// /lambda_comment/ l- ## 0
// /lambda_return/  l- ## s-
// /lambda_bracket/ l- ## 0
// /copy_ctor/      l- ## s-,c-
// /call_copy/      l- ## s-,c-
