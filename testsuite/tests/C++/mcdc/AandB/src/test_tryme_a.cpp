#include <tryme_api.hh>

int
main ()
{
  tryme (/* aa */ true, /* bb */ true);
  tryme (/* aa */ false, /* bb */ true);
}

//# tryme.cpp tryme.hh
// /eval/  l! ## c!:"bb"
// /true/  l+ ## 0
// /false/ l+ ## 0
// /other/ l+ ## 0

// /test_skip/ l! ## dT-
// /skip/      l- ## s-
