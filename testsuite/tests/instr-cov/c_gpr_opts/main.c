extern void hello (const char *who);

void
dummy_putf (const char *fmt, ...)
{
}

int
main (void)
{
#ifdef A
  hello ("A");
#endif

#ifdef B
  hello ("B");
#endif

  return 0;
}
