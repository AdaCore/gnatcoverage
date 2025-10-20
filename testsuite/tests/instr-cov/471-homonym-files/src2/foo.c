extern void print_msg (const char *msg);

void
foo2 (void)
{
  int i;
  for (i = 0; i < 2; ++i)
    {
      print_msg ("Hello from foo2");
    }
}
