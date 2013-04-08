#ifndef SLISTS_FORALL_H
# define SLISTS_FORALL_H

# include "slists.h"

enum sensor_op
{
  SENSOR_ACTIVATE,
  SENSOR_INHIBIT
};

extern void
slist_forall_in (struct sensor_list *l, enum sensor_op op, bool active_only);

#endif
