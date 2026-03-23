extern int fact (int n);
extern void check (int c);

int
main (void)
{
  int n = fact (1);
  check (n == 1);
  return 0;
}
