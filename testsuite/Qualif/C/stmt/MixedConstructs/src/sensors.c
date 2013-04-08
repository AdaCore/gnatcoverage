#include "sensors.h"

void
sensor_init (int lower_bound, int upper_bound, struct sensor *s)
{
  s->value = 0;
  s->lower_bound = lower_bound;
  s->upper_bound = upper_bound;
  s->active = false;
  vbuffer_init (&s->history);
}

void
sensor_sample (struct sensor *s)
{
  vbuffer_push (s->value, &s->history);
}
