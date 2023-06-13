#ifndef FOO_H
#define FOO_H

extern int identity (int x); /* from libsupport.  */

extern void set_current (int p);
extern int get_current (void);
extern int *get_process (int p);

extern int *bar_false (void);
extern int *bar_true (void);

static inline int *
process_current (void)
{
  if (get_current ())                    // # if-expr
    return get_process (get_current ()); // # if-then
  else
    return (int *) 0; // # if-else
}

#endif
