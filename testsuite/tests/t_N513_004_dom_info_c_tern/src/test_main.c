int
check (int argc)
{
  (argc < 10) ? (void) 0 : (void) (argc = argc / 2 + 1); // # tern
  return argc;
}

int
main (int argc, char *argv[])
{
  return check (0);
}

//# test_main.c
//  /tern/ l! ## dF-, s-
