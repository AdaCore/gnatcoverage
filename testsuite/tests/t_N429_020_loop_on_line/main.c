#define MAX_FD 32
#define NULL ((void *) 0)

int
main (int argc, char *argv[])
{
  // void** p = &get_api_data()->file_desc[STDERR_FILENO+1];
  void **p;
  int n = 0;
  for (n = 1; n < MAX_FD && *p != NULL; n++, p++)
    ;
  if (n == MAX_FD)
    {
      n = -1;
    }
  return n;
}
