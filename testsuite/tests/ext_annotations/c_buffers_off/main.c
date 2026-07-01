static void
increment (int *i)
{
  *i += 1;
}

# 8 "main.c"
int
main (void)
{
  int i = -1;
  increment (&i);
  /* GNATCOV_DUMP_BUFFERS */
  return i;
}
