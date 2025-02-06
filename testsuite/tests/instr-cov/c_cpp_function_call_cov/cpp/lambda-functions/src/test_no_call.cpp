#include "pkg.h"

int
main (void)
{
  return 0;
}

//# pkg.cpp
// /.*def_1/        l- ## f-
// /.*def_2/        l- ## 0
// /lambda_assign/  l- ## s-
// /lambda_def/     l- ## f-
// /lambda_comment/ l- ## 0
// /lambda_return/  l- ## s-
// /lambda_bracket/ l- ## 0
// /assignment/     l- ## s-
// /call_copy/      l- ## s-,c-
