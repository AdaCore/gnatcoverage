#include "sensors.h"
#include "slists.h"
#include "slists_forall.h"
#include "support.h"

int
main (void)
{
  struct sensor s;
  struct sensor_list l;

  sensor_init (1, 10, &s);
  s.value = 0;

  slist_init (&l);
  slist_prepend (&s, &l);

  slist_forall_in (&l, SENSOR_INHIBIT, true);
  assert (!s.active);
  return 0;
}

//# slists_forall.c
//  /FA_init/           l+ ## 0
//  /FA_while/          l+ ## 0
//  /FA_tactive/        l+ ## 0
//  /FA_case/           l- ## s-
//  /FA_activate/       l- ## s-
//  /FA_tinhibitLB/     l- ## s-
//  /FA_tinhibitHB/     l- ## 0c
//  /FA_do_inhibit/     l- ## s-
//  /FA_inhibit_end/    l- ## s-
//  /FA_next/           l+ ## 0
