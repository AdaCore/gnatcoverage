namespace foo
{
const float PI = 3.14;
}

// Purposely not called function.
void
bar ()
{
  using namespace foo; // # using

  auto x = PI; // # use_pi
}

// Purposely not called function.
void
baz ()
{
  using foo::PI; // # using

  auto x = PI; // # use_pi
}

int
main (void)
{
  0;
}

//# test_call.cpp
// /using/  l. ## 0
// /use_pi/ l- ## s-
