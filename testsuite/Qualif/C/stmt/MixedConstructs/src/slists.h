#ifndef SLISTS_H
# define SLISTS_H

# include "vbufs.h"
# include "sensors.h"

struct sensor_node;

struct sensor_node
{
  struct sensor *s;
  struct sensor_node *next;
};

struct sensor_list
{
  struct sensor_node *head;
  unsigned len;
};

extern void
slist_init (struct sensor_list *l);

extern void
slist_prepend (struct sensor *s, struct sensor_list *l);

#endif
