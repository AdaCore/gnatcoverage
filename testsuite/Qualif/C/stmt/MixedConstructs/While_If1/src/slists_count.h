#ifndef SLISTS_COUNT_H
# define SLISTS_COUNT_H

#include "sensors.h"
#include "slists.h"
#include "sensors_predicates.h"

extern void
slist_count_in (struct sensor_list *l,
                sensor_predicate p,
                unsigned *count_true, unsigned *count_false);

#endif
