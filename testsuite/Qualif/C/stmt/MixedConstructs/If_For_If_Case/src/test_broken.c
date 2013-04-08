#include "sensors.h"
#include "sensors_status.h"

#include <assert.h>

int
main (void)
{
  struct sensor s;

  sensor_init (1, 10, &s);

  /* The two history entries, both out of range.  */
  s.value = 15;
  sensor_sample (&s);
  sensor_sample (&s);

  assert (sensor_get_status (&s) == SENSOR_BROKEN);

  /* A third entry, in range. Still two out of range.  */
  s.value = 5;
  sensor_sample (&s);
  assert (sensor_get_status (&s) == SENSOR_BROKEN);

  return 0;
}

/* Node that we get into SENSOR_CHECK state internally, before moving to
   SENSOR_BROKEN.  */

//# sensors_status.c
//  /SO_t0/         l+ ## 0
//  /SO_undecide/   l- ## s-
//  /SO_decide/     l+ ## 0
//  /SO_init/       l+ ## 0
//  /SO_loop/       l+ ## 0
//  /SO_tfaultLB/   l+ ## 0
//  /SO_tfaultUB/   l+ ## 0
//  /SO_fault/      l+ ## 0
//  /SO_check/      l+ ## 0
//  /SO_broken/     l+ ## 0
//  /SO_PE/         l- ## s-
//  /SO_iter/       l+ ## 0
//  /SO_ret/        l+ ## 0
