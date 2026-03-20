#include "tryme.hh"

namespace glob
{
bool aa, bb;
}

void
bar (bool b = glob::aa || glob::bb) // # eval :o/e:
{
}

void
tryme (bool aa, bool bb, bool skip)
{
  if (skip)      // # test_skip
    return;      // # skip
  glob::aa = aa; // # other
  glob::bb = bb; // # other
  bar ();        // # other
}
