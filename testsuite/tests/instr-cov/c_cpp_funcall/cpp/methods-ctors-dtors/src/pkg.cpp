#include "pkg.h"

class Point2D
{
  int x = 0;
  int y = 0;

public:
  Point2D () = default;                     // # default_ctor_def
  Point2D (int x, int y) : x (x), y (y) {}; // # var_ctor_def

  Point2D add (Point2D &other) const;
};

Point2D Point2D::add (Point2D &other) const { // # point2d_add_def_1

  // This calls a constructor
  return Point2D (this->x + other.x, this->y + other.y); // # return
}

void foo (void) { // # foo_def_1

  Point2D A;                  // # decl_a
  Point2D B = Point2D (1, 3); // # decl_b

  auto sum = A.add (B); // # sum
}
