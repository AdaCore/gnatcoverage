#pragma once
#include <cstdlib>

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
};

class B
{
  A _a;

public:
  B (A a) : _a (a){};

  A
  get_a () const
  {
    // Purposely exit early after dumping the buffers.
    /* GNATCOV_DUMP_BUFFERS */
    exit (0);
    return this->_a;
  }
};
