extern void dummy_putf (const char *fmt, ...);

void
hello (const char *who)
{
  dummy_putf ("Hello %s\n", who);
}
