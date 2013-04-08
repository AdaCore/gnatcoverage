#include "sensors.h"
#include "slists.h"
#include "slists_count.h"

#include <assert.h>

/* Call "slist_count_in" with an non-empty list and a filter such that the IF
   control evaluates both true and false.  */

int
main (void)
{
  struct sensor s_in, s_out;
  struct sensor_list l;

  unsigned count_false, count_true;

  sensor_init (1, 10, &s_in);
  s_in.value = 5;

  sensor_init (1, 3, &s_out);
  s_out.value = 5;

  slist_init (&l);
  slist_prepend (&s_in, &l);
  slist_prepend (&s_out, &l);

  slist_count_in (&l, sensor_inrange, &count_true, &count_false);
  assert (count_true == 1);
  assert (count_false == 1);

  return 0;
}

//# slists_count.c
//  /CO_decl/   l+ ## 0
//  /CO_init/   l+ ## 0
//  /CO_while/  l+ ## 0
//  /CO_test/   l+ ## 0
//  /CO_incT/   l+ ## 0
//  /CO_intF/   l+ ## 0
//  /CO_next/   l+ ## 0
