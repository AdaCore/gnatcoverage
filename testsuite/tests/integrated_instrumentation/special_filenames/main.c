extern int foo (int a, int b);
extern int bar (int c);

int
main (void)
{
  foo (0, 1);
  bar (0);
  return 0;
}
