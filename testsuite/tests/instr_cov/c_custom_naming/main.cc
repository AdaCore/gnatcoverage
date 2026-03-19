extern void hello (const char *who);

void
dummy_putf (const char *fmt, ...)
{
}

int
main (void)
{
  hello ("world");
  return 0;
}
