#include "sensors.h"
#include "slists.h"
#include "slists_count.h"
#include "support.h"

/* Call "slist_count_in" with an empty list. The WHILE loop is never
   entered.  */

int
main (void)
{
  struct sensor_list l;
  unsigned count_false, count_true;

  slist_init (&l);

  slist_count_in (&l, sensor_pass, &count_true, &count_false);
  assert (count_true == 0);
  assert (count_false == 0);

  return 0;
}

//# slists_count.c
//  /CO_decl/   l+ ## 0
//  /CO_init/   l+ ## 0
//  /CO_while/  l+ ## 0
//  /CO_test/   l- ## s-
//  /CO_incT/   l- ## s-
//  /CO_incF/   l- ## s-
//  /CO_next/   l- ## s-
