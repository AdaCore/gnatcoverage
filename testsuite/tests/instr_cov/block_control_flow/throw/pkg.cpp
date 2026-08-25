#include "pkg.h"

int
throwing (int x)
{
  int r = 0;
  try
    {
      r = 1;
      throw x;
      r = 2;
    }
  catch (int)
    {
      r = 3;
    }
  return r;
}

int
nested_throwing (int x)
{
  int r = 0;
  try
    {
      r = 1;
      int y = x > 0 ? throw x : 0;
      r = 2;
    }
  catch (int)
    {
      r = 3;
    }
  return r;
}

int
capture_throwing (int x)
{
  int r = 0;
  try
    {
      r = 1;
      auto f = [v = (x > 0 ? (throw x, 0) : 0)] { return v; };
      r = f ();
    }
  catch (int)
    {
      r = 3;
    }
  return r;
}

int
lambda_body_throwing (int x)
{
  int r = 0;
  try
    {
      r = 1;
      [x] { throw x; }();
      r = 2;
    }
  catch (int)
    {
      r = 3;
    }
  return r;
}
