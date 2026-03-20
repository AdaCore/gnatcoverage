#include "pkg.h"

template <class T>
void
print_point (Point<T> p)
{
  (void) p;
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
