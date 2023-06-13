extern int foo (int a, int b);

static int
bar (int a, int b)
{
  if (a || b)
    return 1;
  return 0;
}
