int
foo ()
{
#ifdef A
  /* GNATCOV_COV_OFF */
  return 0;
/* GNATCOV_COV_ON */
#endif
#ifdef B
  /* GNATCOV_COV_OFF */
  return 1;
/* GNATCOV_COV_ON */
#endif
  return 2;
}
