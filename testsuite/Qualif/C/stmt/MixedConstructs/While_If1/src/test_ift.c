#include "sensors.h"
#include "slists.h"
#include "slists_count.h"

#include <assert.h>

/* Call "slist_count_in" with an non-empty list and an always accepting
   filter.  The if control must always evaluate to true.  */

int
main (void)
{
  struct sensor s;
  struct sensor_list l;

  unsigned count_false, count_true;

  sensor_init (0, 0, &s);
  slist_init (&l);
  slist_prepend (&s, &l);
  slist_prepend (&s, &l);

  slist_count_in (&l, sensor_pass, &count_true, &count_false);
  assert (count_true == 2);
  assert (count_false == 0);

  return 0;
}

//# slists_count.c
//  /CO_decl/   l+ ## 0
//  /CO_init/   l+ ## 0
//  /CO_while/  l+ ## 0
//  /CO_test/   l+ ## 0
//  /CO_incT/   l+ ## 0
//  /CO_incF/   l- ## s-
//  /CO_next/   l+ ## 0
