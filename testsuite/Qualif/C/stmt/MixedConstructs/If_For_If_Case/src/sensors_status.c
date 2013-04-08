#include "sensors_status.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>

enum sensor_status
sensor_get_status (struct sensor *s)
{
  enum sensor_status status;
  int i;

  if (s->history.len == 0)                          // # SO_t0
    status = SENSOR_UNDECIDABLE;                    // # SO_undecide
  else
    {
      status = SENSOR_OK;                           // # SO_decide

      i = 0;                                        // # SO_init
      while (i < s->history.len)                    // # SO_loop
        {
          if (s->history.store[i] < s->lower_bound  // # SO_tfaultLB
           || s->history.store[i] > s->upper_bound) // # SO_tfaultUB
            {
              switch (status)                       // # SO_fault
                {
                case SENSOR_OK:
                  status = SENSOR_CHECK;            // # SO_check
                  break;                            // # SO_check
                case SENSOR_CHECK:
                case SENSOR_BROKEN:
                  status = SENSOR_BROKEN;           // # SO_broken
                  break;                            // # SO_broken
                default:
                  assert (false);                   // # SO_PE
                  break;
                }
            }
          ++i;                                      // # SO_iter
        }
    }

  return status;                                    // # SO_ret
}
