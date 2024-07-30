static int
static_helper (int *x)
{
  if (!x)     // # exempt_h_d
    return 0; // # exempt_h_s
  return *x;  // # ok
}
