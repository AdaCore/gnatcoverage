#include <tryme_api.hh>

class AndExpr
{
public:
  int
  eval (bool aa, bool bb)
  {
    if (aa && bb)  // # eval :o/d:
      return true; // # true
    else
      return false; // # false
  }
};

void
tryme (bool aa, bool bb, bool skip)
{
  if (skip) // # test_skip
    return; // # skip

  AndExpr ().eval (aa, bb); // # other
}
