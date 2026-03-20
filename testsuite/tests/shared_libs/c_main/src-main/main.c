extern void adainit (void);
extern int mylib__fact (int);

int
main (int argc, const char **argv)
{
  /* Run elaboration for both the runtime and our Ada units.  */
  adainit ();

  /* Run the code to cover.  */
  mylib__fact (1);

  return 0;
}
