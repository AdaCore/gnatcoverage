#include "sum.hh"

int
main (void)
{
  int result;

  try
    {
      result = sum ({ 0, 1 });
    }
  catch (const char *msg)
    {
      return 0;
    }

  return 1;
}

//# sum.cpp
//
//  /init/      l+ ## 0
//  /for-init/  l+ ## 0
//  /for-range/ l- ## s-
//  /add/       l- ## s-
//  /return/    l- ## s-
