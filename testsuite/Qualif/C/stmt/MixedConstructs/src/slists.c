#include "slists.h"
#include "simple_pools.h"
#include "support.h"

void
slist_init (struct sensor_list *l)
{
  l->head = NULL;
  l->len = 0;
}

void
slist_prepend (struct sensor *s, struct sensor_list *l)
{
  struct sensor_node * const node = pool_allocate ();
  node->s = s;
  node->next = l->head;
  l->head = node;
  ++l->len;
}
