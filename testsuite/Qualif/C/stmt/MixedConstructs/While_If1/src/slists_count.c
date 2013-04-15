#include "slists_count.h"
#include "support.h"

void
slist_count_in (struct sensor_list *l,
                sensor_predicate p,
                unsigned *count_true, unsigned *count_false)
{
  struct sensor_node *node = l->head;   // # CO_decl

  *count_true = *count_false = 0;       // # CO_init
  while (node != NULL)                  // # CO_while
    {
      if (p (node->s))                  // # CO_test
        ++*count_true;                  // # CO_incT
      else
        ++*count_false;                 // # CO_incF
      node = node->next;                // # CO_next
    }
}
