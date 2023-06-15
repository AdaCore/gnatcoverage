#include "count_digits.hh"

int
main (void)
{
  volatile bool b = false;
  return b && count_digits ("hello");
}

//# count_digits.cpp
//
//  /init/       l- ## s-
//  /while-cond/ l- ## s-
//  /if-cond/    l- ## s-
//  /if-incr/    l- ## s-
//  /return/     l- ## s-
