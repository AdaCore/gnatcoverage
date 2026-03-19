char
aligned (int x, int y, int factor)
{
  if (!(x & (factor - 1)) && !(y && (factor - 1))) // # decision
    return 1;                                      // # stmt-true
  else
    return 0; // # stmt-false
}
