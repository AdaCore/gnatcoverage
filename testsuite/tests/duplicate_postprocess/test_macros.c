extern int macro_stmts_enabled (void);
extern void macro_stmts (void);

void
dummy_puts (const char *msg)
{
  return;
}

int
main (void)
{
  if (macro_stmts_enabled ())
    macro_stmts ();
  return 0;
}
