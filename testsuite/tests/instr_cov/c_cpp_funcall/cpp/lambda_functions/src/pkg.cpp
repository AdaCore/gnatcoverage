#include "pkg.h"

void foo (int i) { // # foo_def

  auto f =         // # lambda_assign
    [&i] (int x) { // # lambda_def
      // # lambda_comment
      return x + i; // # lambda_return
    }; // # lambda_bracket

  // A call to the copy constructor is made
  auto f_copy = f; // # copy_ctor

  // A copy of the lambda will cover the lambda for function coverage.
  // Also, this call is to be instrumented.
  auto y = f_copy (30); // # call_copy
}
