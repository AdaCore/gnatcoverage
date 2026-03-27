// Do not include pkg.h, to check that main.c is not instrumented yet again
// when pkg.h gets included in the list of units of interest.
extern int foo ();

int
main ()
{
  foo ();
  return 0;
}
