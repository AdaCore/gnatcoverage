#include "sensors.h"
#include "slists.h"
#include "slists_count.h"
#include "support.h"

/* Call "slist_count_in" with an non-empty list and an always rejecting
   filter.  The if control must always evaluate to false.  */

int
main (void)
{
  struct sensor s;
  struct sensor_list l;

  unsigned count_false, count_true;

  sensor_init (0, 0, &s);
  slist_init (&l);
  slist_prepend (&s, &l);

  slist_count_in (&l, sensor_nopass, &count_true, &count_false);
  assert (count_true == 0);
  assert (count_false == 1);

  return 0;
}

//# slists_count.c
//  /CO_decl/   l+ ## 0
//  /CO_init/   l+ ## 0
//  /CO_while/  l+ ## 0
//  /CO_test/   l+ ## 0
//  /CO_incT/   l- ## s-
//  /CO_incF/   l+ ## 0
//  /CO_next/   l+ ## 0
