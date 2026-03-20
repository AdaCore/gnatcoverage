extern void foo (void (*f) (const char *str));

static void
callback (const char *str)
{
}

int
main (void)
{
  foo (callback);
  return 0;
}
