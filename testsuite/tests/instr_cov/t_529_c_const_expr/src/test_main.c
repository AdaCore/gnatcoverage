struct t_struct
{
  int x;
  int y;
};

// This is currently some no code because we don't create SCOs for it
// If that was to change, it should be marked "undetermined coverage",
// like the others below.
const int MY_GLOBAL = sizeof (long) == 4 ? 1 : 2;

int
test ()
{
  static const short values[] = {
    [0] = 1,
    [1] = sizeof (long) == 4 ? 1 : 2, // # static-decision
  };

  static const short ignored[sizeof (long) == 4 ? 1 : 2] // # static-decision
    = { [0] = 2 };

  static short not_const[sizeof (long) == 4 ? 1 : 2] // # static-decision
    = { [0] = 2 };

  static const struct t_struct t = {
    .x = 0,
    .y = sizeof (long) == 4 ? 1 : 2 // # static-decision
  };

  return values[1] + t.y;
}

int
main ()
{
  return test () & 0;
}

//# test_main.c
// /static-decision/ l? ## d?
