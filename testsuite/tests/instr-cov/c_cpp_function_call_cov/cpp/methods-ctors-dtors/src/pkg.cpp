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

Point2D                             // # point2d_add_def_1
Point2D::add (Point2D &other) const // # point2d_add_def_2
{                                   // # point2d_add_def_2

  // This calls a constructor
  return Point2D (this->x + other.x, this->y + other.y); // # return
}

void       // # foo_def_1
foo (void) // # foo_def_2
{          // # foo_def_2

  Point2D A;                  // # point_a
  Point2D B = Point2D (1, 3); // # point_b

  auto sum = A.add (B); // # sum
}
