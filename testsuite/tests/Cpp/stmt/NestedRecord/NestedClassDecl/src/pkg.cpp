
int
return_number (void)
{
  class NumberClass
  {
  public:
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
