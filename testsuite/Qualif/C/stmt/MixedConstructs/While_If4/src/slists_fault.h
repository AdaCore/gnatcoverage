#ifndef SLISTS_FAULT
# define SLISTS_FAULT

#include "slists.h"

#include <stdbool.h>

extern void
slist_control (struct sensor_list *s, bool active_only,
               struct sensor_list *skipped,
               struct sensor_list *fault,
               struct sensor_list *ok);

#endif
