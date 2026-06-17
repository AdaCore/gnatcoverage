#define B 1

#include "pkg.h"

int
main ()
{
  return 0; // # covered
}

//# pkg.h
//
//  /cov-off-b/ lD ## dB:"No justification"
//  /cov-on/    l- ## s-
//
//# test_b.c
//
//  /covered/   l+ ## 0
