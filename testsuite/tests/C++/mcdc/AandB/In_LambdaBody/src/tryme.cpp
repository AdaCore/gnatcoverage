void
tryme (bool aa, bool bb, bool skip)
{
  if (skip) // # test_skip
    return; // # skip

  // A lambda function with a real sequence of statements here

  auto AndExpr = [] (int aa, int bb) { // # other
    if (aa > 0 && bb > 0)              // # eval :o/d:
      return true;                     // # lambda_true
    else                               // # lambda_other
      return false;                    // # lambda_false
  }; // # lambda_other

  volatile bool b = AndExpr (aa, bb); // # other
}
