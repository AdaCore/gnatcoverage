extern int foo (int i);
extern int bar (int a, int b, int c);

int
main (void)
{
  int result = foo (0) + bar (1, 0, 2);
  return result;
}
