#include "slists_forall.h"

#include <stdlib.h>

void
slist_forall_in (struct sensor_list *l, enum sensor_op op, bool active_only)
{
  struct sensor_node *node = l->head;                   // # FA_init

  while (node != NULL)                                  // # FA_while
    {
      if (node->s->active || !active_only)              // # FA_tactive
        switch (op)                                     // # FA_case
        {
        case SENSOR_ACTIVATE:
          node->s->active = true;                       // # FA_activate
          break;                                        // # FA_activate
        case SENSOR_INHIBIT:
          if (node->s->value < node->s->lower_bound     // # FA_tinhibitLB
           || node->s->value > node->s->upper_bound)    // # FA_tinhibitHB
            node->s->active = false;                    // # FA_do_inhibit
          break;                                        // # FA_inhibit_end
        }
      node = node->next;                                // # FA_next
    }
}
