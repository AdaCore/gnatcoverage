#define A

#include "pkg.h"

int
main ()
{
  return 0; // # covered
}

//# pkg.h
//
//  /cov-off-a/ lD ## dB:"No justification"
//  /cov-on/    l- ## s-
//
//# test_a.c
//
//  /covered/   l+ ## 0
