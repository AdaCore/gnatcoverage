void
print (const char *msg)
{
}

int
main (void)
{
  int i = 0;

  /* GNATCOV_EXEMPT_BRANCH("J") */
  if (i == 0)
    {
      print ("Null");
      /* GNATCOV_EXEMPT_BRANCH("J") */
    }
  /* GNATCOV_EXEMPT_BRANCH("J") */

  i = 10;

  /* GNATCOV_EXEMPT_BRANCH("J") */
  return 0;
}
