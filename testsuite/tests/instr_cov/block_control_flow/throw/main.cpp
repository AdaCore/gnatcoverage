#include "pkg.h"

int
main (void)
{
  throwing (1);
  nested_throwing (1);
  capture_throwing (1);
  lambda_body_throwing (1);
  return 0;
}
