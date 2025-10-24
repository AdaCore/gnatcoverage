extern void print_msg (const char *msg);
extern int foo (int a, int b);

int
main (int argc, char *argv[])
{
  if (foo (1, 2))
    print_msg ("foo returned true");
  return 0;
}
