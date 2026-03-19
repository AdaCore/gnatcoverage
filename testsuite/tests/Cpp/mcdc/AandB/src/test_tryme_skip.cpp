#include <tryme_api.hh>

int
main ()
{
  tryme (/* aa */ true, /* bb */ true, /* skip */ true);
}

//# tryme.cpp tryme.hh
// /eval/  l- ## s-
// /true/  l- ## s-
// /false/ l- ## s-
// /other/ l- ## s-

// All the lines of a stmt involving a lambda expr
// are noted '-' and the ones marked "other" don't
// have a nested entity of their own

// /lambda_other/  l- ## 0c
// /lambda_true/   l- ## s-
// /lambda_false/  l- ## s-

// /test_skip/ l! ## dF-
// /skip/      l+ ## 0
