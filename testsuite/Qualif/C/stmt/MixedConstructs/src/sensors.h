#ifndef SENSORS_H
# define SENSORS_H

# include "vbufs.h"

# include <stdbool.h>

struct sensor
{
  int value;
  int lower_bound;
  int upper_bound;
  bool active;
  struct vbuffer history;
};

extern void
sensor_init (int lower_bound, int upper_bound, struct sensor *s);

/* Latch current sensor value for S in its history buffer.  */
extern void
sensor_sample (struct sensor *s);

#endif
