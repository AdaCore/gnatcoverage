#ifndef SENSORS_PREDICATES_H
# define SENSORS_PREDICATES_H

# include "sensors.h"

extern bool
sensor_nopass (struct sensor *s);

extern bool
sensor_pass (struct sensor *s);

extern bool
sensor_inrange (struct sensor *s);

typedef bool (*sensor_predicate)(struct sensor *s);

#endif
