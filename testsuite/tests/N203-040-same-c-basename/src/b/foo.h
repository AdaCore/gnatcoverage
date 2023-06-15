static int
other_foo (int a)
{
  if (a)          // # other-stmt
    return 2 * a; // # other-true
  else
    return a / 2; // # other-false
}
