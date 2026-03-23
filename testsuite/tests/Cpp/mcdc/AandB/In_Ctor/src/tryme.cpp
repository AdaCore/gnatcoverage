#include <tryme_api.hh>

class AndExpr
{
  bool _value;

public:
  AndExpr (bool aa, bool bb)
  {
    _value = aa && bb; // # eval :o/e:
  }
};

void
tryme (bool aa, bool bb, bool skip)
{
  if (skip) // # test_skip
    return; // # skip

  AndExpr (aa, bb); // # other
}
