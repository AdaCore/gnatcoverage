
int
fact (int n)
{
  if (n == 0)
    {
      return 1;
    }
  return n * fact (n - 1);
}

int
main (void)
{
  return
    /* ***************ON "In statement"*/
    fact (0) == 1
      /* ***************OFF)*/
      ? 0
      : 1;
}
