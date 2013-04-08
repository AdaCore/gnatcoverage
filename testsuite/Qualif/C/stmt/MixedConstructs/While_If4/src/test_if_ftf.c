#include "sensors.h"
#include "slists_fault.h"

#include <assert.h>

int
main (void)
{
  struct sensor s;
  struct sensor_list l;
  struct sensor_list skipped, fault, ok;

  sensor_init (1, 1, &s);
  s.value = 5;
  s.active = true;

  slist_init (&l);
  slist_prepend (&s, &l);

  slist_init (&skipped);
  slist_init (&fault);
  slist_init (&ok);

  slist_control (&l, true, &skipped, &fault, &ok);

  assert (skipped.len == 0);
  assert (fault.len == 1);
  assert (ok.len == 0);
  return 0;
}

//# slists_fault.c
//  /AF_init/   l+ ## 0
//  /AF_while/  l+ ## 0
//  /AF_evA/    l+ ## 0
//  /AF_skip/   l- ## s-
//  /AF_evLB/   l+ ## 0
//  /AF_evHB/   l+ ## 0
//  /AF_fault/  l+ ## 0
//  /AF_ok/     l- ## s-
//  /AF_next/   l+ ## 0
