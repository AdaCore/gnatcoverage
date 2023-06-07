extern int one_int (void);
extern unsigned one_unsigned (void);

int
main (void)
{
  return one_unsigned () - one_int ();
}

//# foo.c
