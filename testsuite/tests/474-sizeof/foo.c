void
print_msg (const char *msg)
{
  (void) msg;
}

int
foo (int a, int b)
{
  return sizeof (a && b);
}
