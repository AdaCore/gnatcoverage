#include "sensors.h"
#include "slists.h"
#include "slists_forall.h"
#include "support.h"

int
main (void)
{
  struct sensor s1, s2, s3;
  struct sensor_list l;

  sensor_init (1, 10, &s1);
  s1.value = 5; /* in range */
  s1.active = true;
  sensor_init (5, 15, &s2);
  s2.value = 1; /* < low bound */
  s2.active = true;
  sensor_init (5, 15, &s3);
  s3.value = 45; /* > high bound */
  s3.active = true;

  slist_init (&l);
  slist_prepend (&s1, &l);
  slist_prepend (&s2, &l);
  slist_prepend (&s3, &l);

  slist_forall_in (&l, SENSOR_INHIBIT, false);
  assert (s1.active);
  assert (!s2.active);
  assert (!s3.active);
  return 0;
}

//# slists_forall.c
//  /FA_init/           l+ ## 0
//  /FA_while/          l+ ## 0
//  /FA_tactive/        l+ ## 0
//  /FA_case/           l+ ## 0
//  /FA_activate/       l- ## s-
//  /FA_tinhibitLB/     l+ ## 0
//  /FA_tinhibitHB/     l+ ## 0
//  /FA_do_inhibit/     l+ ## 0
//  /FA_inhibit_end/    l+ ## 0
//  /FA_next/           l+ ## 0
