extern int func1 (int);
extern int func2 (int);
extern void putchar (int);

void
put_digit (int a)
{
  putchar ('0' + a);
}

void
put_str (char *s)
{
  while (*s)
    putchar (*s++);
}

int
c_main (void)
{
  put_str ("func2: ");
  put_digit (func2 (1));
  putchar ('\n');
  put_str ("func1: ");
  put_digit (func1 (2));
  putchar ('\n');

  return 0;
}
