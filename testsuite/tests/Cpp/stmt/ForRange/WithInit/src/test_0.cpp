#include "sum.hh"

int
main (void)
{
  volatile bool b = false;
  return b && sum ({ 0, 1 });
}

//# sum.cpp
//
//  /init/      l- ## s-
//  /for-init/  l- ## s-
//  /for-range/ l- ## s-
//  /add/       l- ## s-
//  /return/    l- ## s-
