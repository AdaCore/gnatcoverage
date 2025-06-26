void
tryme (bool aa, bool bb, bool skip)
{
  if (skip) // # test_skip
    return; // # skip

  // Basic lambda function here, a mere expression return

  auto AndExpr = [] (int aa, int bb) { // # other
    return aa > 0 && bb > 0;           // # eval :o/e:
  }; // # lambda_other

  volatile bool b = AndExpr (aa, bb); // # other
}
