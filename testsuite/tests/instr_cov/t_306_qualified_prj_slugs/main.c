extern void foo1 (void);
extern void foo2 (void);

void
print_msg (const char *msg)
{
  (void) msg;
}

int
main (void)
{
  foo1 ();
  foo2 ();
  return 0;
}
