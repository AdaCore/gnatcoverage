#include "gnatcov_rts_c-buffers.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>

extern struct gnatcov_rts_coverage_buffers_group *BUFFERS[];

void
dump_buffers (void)
{
  struct gnatcov_rts_coverage_buffers_group **current = BUFFERS;
  while (*current != NULL)
    {
      unsigned group_len = (*current)->length;
      const struct gnatcov_rts_coverage_buffers **buffer_group
        = (*current)->buffers;

      // Assume group_len >= 1
      struct gnatcov_rts_string unit_name = buffer_group[0]->unit_name;
      printf ("%.*s\n", unit_name.length, unit_name.str);

      // If group_len > 1, check that others buffers are for the same unit
      unsigned b;
      for (b = 1; b < group_len; b++)
        {
          struct gnatcov_rts_string next_unit = buffer_group[b]->unit_name;
          assert (unit_name.length == next_unit.length);
          assert (strncmp (unit_name.str, next_unit.str, unit_name.length)
                  == 0);
        }

      current++;
    }
}
