extern int absval (int x);

void
test_abs (void)
{
  volatile int x = -5;
  volatile int y = absval (x);
}

int
main (void)
{
  test_abs ();
  return 0;
}
