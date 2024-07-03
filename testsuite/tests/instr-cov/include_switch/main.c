int
main (void)
{
  hello_from_pkg1 ();
  hello_from_pkg2 ();
  hello_from_pkg3 ();
  return 0;
}

void
print_msg (const char *msg)
{
  (void *) msg;
}
