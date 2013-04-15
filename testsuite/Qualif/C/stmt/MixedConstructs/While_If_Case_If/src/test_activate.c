#include "sensors.h"
#include "slists.h"
#include "slists_forall.h"
#include "support.h"

int
main (void)
{
  struct sensor s1, s2;
  struct sensor_list l;

  sensor_init (1, 10, &s1);
  sensor_init (5, 15, &s2);

  slist_init (&l);
  slist_prepend (&s1, &l);
  slist_prepend (&s2, &l);

  slist_forall_in (&l, SENSOR_ACTIVATE, true);
  assert (!s1.active);
  assert (!s2.active);

  slist_forall_in (&l, SENSOR_ACTIVATE, false);
  assert ( s1.active);
  assert ( s2.active);
  return 0;
}

//# slists_forall.c
//  /FA_init/           l+ ## 0
//  /FA_while/          l+ ## 0
//  /FA_tactive/        l+ ## 0
//  /FA_case/           l+ ## 0
//  /FA_activate/       l+ ## 0
//  /FA_tinhibitLB/     l- ## s-
//  /FA_tinhibitHB/     l- ## 0c
//  /FA_do_inhibit/     l- ## s-
//  /FA_inhibit_end/    l- ## s-
//  /FA_next/           l+ ## 0
