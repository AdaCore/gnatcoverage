int
foo (int a, int b)
{
#ifdef A
  /* GNATCOV_COV_OFF */ // # cov-off-a
  int c = a && b;       // # cov-off-a
  /* GNATCOV_COV_ON */  // # cov-off-a
#endif

#ifdef B
  /* GNATCOV_COV_OFF */ // # cov-off-b
  int d = a && b;       // # cov-off-b
  /* GNATCOV_COV_ON */  // # cov-off-b
#endif

  return 0; // # cov-on
}
