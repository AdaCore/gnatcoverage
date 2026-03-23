extern "C"
{
  bool
  AndExpr (bool aa, bool bb)
  {
    return aa && bb; // # eval :o/e:
  }
}

void
tryme (bool aa, bool bb, bool skip)
{
  if (skip) // # test_skip
    return; // # skip

  volatile bool ev = AndExpr (aa, bb); // # other
}
