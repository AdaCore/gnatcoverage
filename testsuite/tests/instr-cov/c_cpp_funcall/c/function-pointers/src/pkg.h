#ifndef _PKG_H
#define _PKG_H

typedef int (*fun_type) (int);

int add_one (int i);
int add_two (int i);

void foo (fun_type);

#endif // _PKG_H
