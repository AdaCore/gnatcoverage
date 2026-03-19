// Recursive constexpr function
constexpr int
fac (int n)
{
  return n == 1 ? 1 : n * fac (n - 1); // # cstexpr
}

int
main (int argc, char *argv[])
{
  bool i = fac (3) == 6; // # check
  return 0;              // # check
}

//# test_main.cpp

// /check/    l+ ## 0
// /cstexpr/ l? ## s?
