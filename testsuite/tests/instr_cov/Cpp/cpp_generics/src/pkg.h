#ifndef _PKG_H
#define _PKG_H

void foo_int (void);
void foo_float (void);

template <class T> struct Point
{
  T x;
  T y;

  Point (T x, T y) : x (x), y (y) {}
};

template <class T> void print_point (Point<T> p);

#endif // _PKG_H
