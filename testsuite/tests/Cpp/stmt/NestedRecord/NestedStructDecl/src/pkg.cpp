
int
return_number (void)
{
  struct NumberClass
  {
    int
    first ()
    {
      return 1; // # one
    }

    int
    second ()
    {
      return 2; // # two
    }
  };

  return NumberClass ().first () + NumberClass ().second (); // # return
}
