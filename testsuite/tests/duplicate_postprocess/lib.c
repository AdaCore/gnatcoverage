extern void dummy_puts (const char *msg);

#define PRINT_HW dummy_puts ("Hello world!\n");

/* Thanks to macro_stmts_enabled returning 0, macro_stmts is never executed,
   and thus we get coverage violations inside macro expansion.  */

int
macro_stmts_enabled (void)
{
  return 0;
}

void
macro_stmts (void)
{
  PRINT_HW;
  PRINT_HW;
}
