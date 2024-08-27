int
foo (int a, int b)
{
#ifdef A
  /* PLACEHOLDER    */ // # cov-off-a
  int c = a && b;      // # cov-off-a
  /* PLACEHOLDER    */ // # cov-off-a
#endif

#ifdef B
  /* PLACEHOLDER     */ // # cov-off-b
  int d = a && b;       // # cov-off-b
  /* PLACEHOLDER     */ // # cov-off-b
#endif

  return 0; // # cov-on
}
