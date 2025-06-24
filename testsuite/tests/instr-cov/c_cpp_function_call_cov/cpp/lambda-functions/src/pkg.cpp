#include "pkg.h"

void        // # foo_def_1
foo (int i) // # foo_def_2
{           // # foo_def_2

  auto f =         // # lambda_assign
    [&i] (int x) { // # lambda_def
      // # lambda_comment
      return x + i; // # lambda_return
    }; // # lambda_bracket

  // This should not be instrumented as a call even though under the hood, a
  // call to a copy constructor is made for f.
  auto f_copy = f; // # assignment

  // A copy of the lambda will cover the lambda for function coverage.
  // Also, this call is to be instrumented.
  auto y = f_copy (30); // # call_copy
}
