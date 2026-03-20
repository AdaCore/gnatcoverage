int
fact (int a)
{
  if (a == 1)
    return 1;
  return a * fact (a - 1);
}

int
main (void)
{
  int dummy = fact (6);
  if (!dummy)
    return 0;
}
