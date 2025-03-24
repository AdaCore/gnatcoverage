class A
{
  int _x;

public:
  A (int x) : _x (x){};

  int
  get_x () const
  {
    return this->_x;
  }

  int
  get_x_plus_one () const
  {
    return get_x () + 1; // # non-prefix-call
  }
};

int
main (void)
{
  A (5).get_x_plus_one (); // # method-call
}

//# test_non_prefix.cpp
// /non-prefix-call/ l+ ## 0
// /method-call/     l+ ## 0
