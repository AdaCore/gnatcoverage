static int
foo (int a)
{
  if (a)          // # toplev-stmt
    return a + 1; // # toplev-true
  else
    return a - 1; // # toplev-false
}
