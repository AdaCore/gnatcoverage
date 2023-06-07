extern void dummy_putf (const char *fmt, ...);

void
hello (const char *who)
{
#ifdef A
  dummy_putf ("Hello %s\n", who);
#endif

#ifdef B
  dummy_putf ("Goodbye %s\n", who);
#endif
}
