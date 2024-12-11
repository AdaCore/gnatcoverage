#include <stdio.h>

extern int mylib__identity (int);

static void
assert_equal (const char *label, int expected, int actual)
{
  const char *status = (expected == actual) ? "OK  " : "FAIL";
  printf ("%s %s\n", status, label);
}

int
main (void)
{
  assert_equal ("Identity (0)", 0, mylib__identity (0));
  assert_equal ("Identity (1)", 1, mylib__identity (1));
  return 0;
}
