int
main (void)
{
  /*GNATCOV_INVALID_KIND*/
  //GNATCOV_EXEMPT_ON(|)
  // GNATCOV_EXEMPT_ON(,)
  // GNATCOV_EXEMPT_ON(foo=(a, b)
  // GNATCOV_EXEMPT_ON(invalid_arg)
  /* GNATCOV_EXEMPT_ON("too", "many", "args") */
  // GNATCOV_EXEMPT_ON(no_such_arg="too")
  // GNATCOV_EXEMPT_ON "legacy"
  // GNATCOV_EXEMPT_ON(100000000000000000000000000000)
  return 0;
  // GNATCOV_EXEMPT_OFF
}
