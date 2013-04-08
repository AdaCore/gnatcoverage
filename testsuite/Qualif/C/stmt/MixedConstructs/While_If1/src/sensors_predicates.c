#include "sensors_predicates.h"

bool
sensor_nopass (struct sensor *s)
{
  return false;
}

bool
sensor_pass (struct sensor *s)
{
  return true;
}

bool
sensor_inrange (struct sensor *s)
{
  return s->lower_bound <= s->value
      && s->value <= s->upper_bound;
}
