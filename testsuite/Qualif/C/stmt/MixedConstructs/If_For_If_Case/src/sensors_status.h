#ifndef SENSORS_STATUS_H
# define SENSORS_STATUS_H

#include "sensors.h"

enum sensor_status
{
  SENSOR_UNDECIDABLE,
  SENSOR_OK,
  SENSOR_CHECK,
  SENSOR_BROKEN
};

extern enum sensor_status
sensor_get_status (struct sensor *s);

#endif
