#include <tryme_api.hh>

int
main ()
{
  tryme (/* aa */ true, /* bb */ true);
  tryme (/* aa */ true, /* bb */ false);
  tryme (/* aa */ false, /* bb */ false);
}

//# tryme.cpp tryme.hh
// /eval/  l+ ## 0
// /true/  l+ ## 0
// /false/ l+ ## 0
// /other/ l+ ## 0

// /test_skip/ l! ## dT-
// /skip/      l- ## s-
