#ifndef _PKG_H
#define _PKG_H

void foo (void);

enum My_Bool
{
  True,
  False,
};

// Overload the comma operator
bool operator|| (My_Bool L, My_Bool R);

#endif // _PKG_H
