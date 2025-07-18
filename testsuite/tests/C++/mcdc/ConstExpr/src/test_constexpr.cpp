#include "pkg.hh"

constexpr int
foo ()
{
  if (true && false)       // # ce-body
    return 1;              // # ce-body
  constexpr bool a = true; // # ce-body
  if constexpr (false)     // # ce-body
    return 1;              // # ce-body
  return 0;                // # ce-body
}

int
main ()
{
  constexpr bool a = true || false;                    // # single-ce-decl
  constexpr bool b = true || false, c = true || false; // # double-ce-decl
  if constexpr (false)                                 // # if-ce
    return 1;                                          // # if-rt
  return 0;                                            // # rt
}

//# test_constexpr.cpp
//
//    /ce-body/        l. ## 0
//    /single-ce-decl/ l+ ## 0
//    /double-ce-decl/ l+ ## 0
//    /if-ce/          l+ ## 0
//    /if-rt/          l- ## s-
//    /rt/             l+ ## 0
