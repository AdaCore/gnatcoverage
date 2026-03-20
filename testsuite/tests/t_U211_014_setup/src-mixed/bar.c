typedef void (*put_char_ptr) (char c);

void
bar (put_char_ptr put_char)
{
  const char *message = "Hello, world!";
  int i;

  for (i = 0; message[i]; ++i)
    {
      put_char (message[i]);
    }
}
