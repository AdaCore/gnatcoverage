#include "sensors.h"
#include "slists.h"
#include "slists_forall.h"

#include <assert.h>

int
main (void)
{
  struct sensor s;
  struct sensor_list l;

  sensor_init (1, 10, &s);
  s.value = 5;
  s.active = true;

  slist_init (&l);
  slist_prepend (&s, &l);

  slist_forall_in (&l, SENSOR_INHIBIT, false);
  assert (s.active);
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
//  /FA_do_inhibit/     l- ## s-
//  /FA_inhibit_end/    l+ ## 0
//  /FA_next/           l+ ## 0
