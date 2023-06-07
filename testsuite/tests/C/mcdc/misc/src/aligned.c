
char
aligned (int x, int y, int factor)
{
  return !(x & (factor - 1)) && !(y & (factor - 1)); // # eval
}
