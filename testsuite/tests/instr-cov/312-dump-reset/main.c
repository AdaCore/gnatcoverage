extern void stub_puts (const char *);

int
main (void)
{
  stub_puts ("This should be covered in first dump");

  /* GNATCOV_DUMP_BUFFERS */

  /* GNATCOV_RESET_BUFFERS */

  stub_puts ("This should be covered in second dump");

  return 0;
}
