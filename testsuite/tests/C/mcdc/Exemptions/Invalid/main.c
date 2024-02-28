
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
    /* GNATCOV_EXEMPT_ON "In statement"*/
    fact (0) == 1
      /* GNATCOV_EXEMPT_OFF)*/
      ? 0
      : 1;
}
