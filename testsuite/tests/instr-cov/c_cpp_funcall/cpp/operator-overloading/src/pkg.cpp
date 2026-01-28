#include "pkg.h"

bool                                               // # operator_def_1
operator|| (My_Bool L, My_Bool R)                  // # operator_def_2
{                                                  // # operator_def_2
  return L == My_Bool::True || R == My_Bool::True; // # return
}

void       // # foo_def_1
foo (void) // # foo_def_2
{          // # foo_def_2

  auto A = My_Bool::True;  // # var_decl
  auto B = My_Bool::False; // # var_decl

  auto res =  // # assignment
    A || B    // # or_my_bool
    || false; // # or_std_bool
}
