void
increment (int *j)
{
  (*j)++;
}

int
main (void)
{
  int i = 1;

  if (i + 1 == 1)
    {
      i++;
    }

  /* GNATCOV_DUMP_BUFFERS ("manual_dump") */
  /* GNATCOV_RESET_BUFFERS */

  increment (&i);
  i += 42;
  i += 1;

  return 0;
}
