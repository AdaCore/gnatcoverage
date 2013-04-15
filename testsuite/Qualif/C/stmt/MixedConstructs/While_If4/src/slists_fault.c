#include "slists_fault.h"
#include "support.h"

void
slist_control (struct sensor_list *l, bool active_only,
               struct sensor_list *skipped,
               struct sensor_list *fault,
               struct sensor_list *ok)
{
  struct sensor_node *node = l->head;               // # AF_init

  while (node != NULL)                              // # AF_while
  {
    if (active_only && !node->s->active)            // # AF_evA
      slist_prepend (node->s, skipped);             // # AF_skip
    else if (node->s->value < node->s->lower_bound  // # AF_evLB
          || node->s->value > node->s->upper_bound) // # AF_evHB
      slist_prepend (node->s, fault);               // # AF_fault
    else
      slist_prepend (node->s, ok);                  // # AF_ok
    node = node->next;                              // # AF_next
  }
}
