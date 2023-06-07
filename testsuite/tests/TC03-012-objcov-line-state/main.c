int
add_n (int a, int b)
{
  int res = b;
  for (int x = 0; x < a; x++)
    {
      res += 1;
    }
  return res;
}

int
main ()
{
  return add_n (0, 0);
}