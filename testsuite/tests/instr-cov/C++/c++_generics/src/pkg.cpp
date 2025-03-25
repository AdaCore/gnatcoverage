#include "pkg.h"
#include <iostream>

template <class T>
void
print_point (Point<T> p)
{
  std::cout << p.x << '\n' << p.y << std::endl;
}

void
foo_int (void)
{
  Point<int> A = Point (1, 2);
  print_point (A);
}

void
foo_float (void)
{
  Point<float> A = Point (1.5f, 2.5f);
  print_point (A);
}
