int
fact (int n)
{
  if (n == 0)
    return 1;
  else if (n < 2)
    return 1;
  else
    return n * fact (n - 1);
}
