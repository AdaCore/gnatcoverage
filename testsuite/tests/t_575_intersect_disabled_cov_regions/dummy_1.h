static int
set_one (int *i_ptr)
{
  *i_ptr = 0;
  // GNATCOV_COV_OFF ("J1")
  ++*i_ptr;
  return 0;
}
