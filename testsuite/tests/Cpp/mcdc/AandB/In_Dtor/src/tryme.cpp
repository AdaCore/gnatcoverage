class AndExpr
{
public:
  AndExpr (bool a, bool b);
  ~AndExpr ();

private:
  bool aa, bb;
};

AndExpr::AndExpr (bool a, bool b)
{
  this->aa = a; // # other
  this->bb = b; // # other
}

static bool evalue;

AndExpr::~AndExpr ()
{
  if (aa && bb)      // # eval :o/d:
    ::evalue = true; // # true
  else
    ::evalue = false; // # false
}

void
tryme (bool aa, bool bb, bool skip)
{
  if (skip) // # test_skip
    return; // # skip

  AndExpr e (aa, bb); // # other
}
