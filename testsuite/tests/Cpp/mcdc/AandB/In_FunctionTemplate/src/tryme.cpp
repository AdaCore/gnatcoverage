template <typename T>
bool
AndExpr (T aa, T bb)
{
  return aa && bb; // # eval :o/e:
}

void
tryme (bool aa, bool bb, bool skip)
{
  if (skip) // # test_skip
    return; // # skip

  volatile bool b = AndExpr<bool> (aa, bb); // # other
}
