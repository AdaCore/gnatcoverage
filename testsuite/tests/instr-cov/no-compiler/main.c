extern void adainit (void);
extern void adafinal (void);
extern void pkg_foo (void);

int
main (void)
{
  adainit ();
  pkg_foo ();
  adafinal ();
  return 0;
}
