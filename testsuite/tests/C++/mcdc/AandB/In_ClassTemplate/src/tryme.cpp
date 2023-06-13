template <bool &aa, bool &bb> class AndExpr
{
  bool _ev;

public:
  void
  eval ()
  {
    _ev = aa && bb; // # eval :o/e:
  }
};

static bool a;
bool b;

void
tryme (bool aa, bool bb, bool skip)
{
  if (skip) // # test_skip
    return; // # skip

  a = aa;                   // # other
  b = bb;                   // # other
  AndExpr<a, b> ().eval (); // # other
}
