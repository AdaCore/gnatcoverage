void
tryme (bool aa, bool bb, bool skip)
{
  if (skip) // # test_skip
    return; // # skip

  // A lambda function in a compound statement
  {
    auto InCompound = [] (int aa, int bb) { // # other
      if (aa > 0 && bb > 0)                 // # eval :o/d:
        return true;                        // # lambda_true
      else                                  // # lambda_other
        return false;                       // # lambda_false
    }; // # lambda_other
    volatile bool a = InCompound (aa, bb); // # other
  }

  // A lambda function in a for statement
  int i = 0;                // # other
  for (; i++ < 1;)          // # other
    [] (int aa, int bb) {   // # other
      if (aa > 0 && bb > 0) // # eval :o/d:
        return true;        // # lambda_true
      else                  // # lambda_other
        return false;       // # lambda_false
    }(aa, bb); // # lambda_other

  // A nested lambda
  [] (int aa, int bb) {     // # other
    [] (int aa, int bb) {   // # other
      if (aa > 0 && bb > 0) // # eval :o/d:
        return true;        // # lambda_true
      else                  // # lambda_other
        return false;       // # lambda_false
    }(aa, bb); // # lambda_other
  }(aa, bb); // # lambda_other
}
