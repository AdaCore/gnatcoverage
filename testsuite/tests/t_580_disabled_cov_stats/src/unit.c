void
c_disabled (int *i)
{
  // GNATCOV_COV_OFF("test")
  ++*i;
  // GNATCOV_COV_ON()
}
