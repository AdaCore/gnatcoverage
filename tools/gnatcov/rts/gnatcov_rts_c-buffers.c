/****************************************************************************
 *                                                                          *
 *                   GNATcoverage Instrumentation Runtime                   *
 *                                                                          *
 *                     Copyright (C) 2021-2024, AdaCore                     *
 *                                                                          *
 * GNATcoverage is free software; you can redistribute it and/or modify it  *
 * under terms of the GNU General Public License as published by the  Free  *
 * Software  Foundation;  either version 3,  or (at your option) any later  *
 * version. This software is distributed in the hope that it will be useful *
 * but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- *
 * TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public *
 * License for  more details.  You should have  received  a copy of the GNU *
 * General  Public  License  distributed  with  this  software;   see  file *
 * COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy *
 * of the license.                                                          *
 *                                                                          *
 ****************************************************************************/

#include "gnatcov_rts_c-buffers.h"
#include "gnatcov_rts_c-memory.h"
#include <stddef.h>
#include <stdint.h>

uint64_t
gnatcov_rts_sum_buffer_bits (
  const struct gnatcov_rts_coverage_buffers_group_array *arr)
{
  if (arr == NULL)
    return 0;

  unsigned i, j, sum = 0;
  int x;
  for (i = 0; i < arr->length; i++)
    {
      const struct gnatcov_rts_coverage_buffers_group *buf_grp
        = arr->groups[i];

      if (buf_grp == NULL)
        return 0;

      for (j = 0; j < buf_grp->length; j++)
        {
          const struct gnatcov_rts_coverage_buffers *unit_bufs
            = buf_grp->buffers[j];

          /* Statement buffer */
          if (unit_bufs->statement != NULL)
            {
              for (x = 0; x < unit_bufs->statement_last_bit + 1; x++)
                {
                  sum += unit_bufs->statement[x];
                }
            }

          /* Decision buffer */
          if (unit_bufs->decision != NULL)
            {
              for (x = 0; x < unit_bufs->decision_last_bit + 1; x++)
                {
                  sum += unit_bufs->decision[x];
                }
            }

          /* MCDC buffer */
          if (unit_bufs->mcdc != NULL)
            {
              for (x = 0; x < unit_bufs->mcdc_last_bit + 1; x++)
                {
                  sum += unit_bufs->mcdc[x];
                }
            }
        }
    }

  return sum;
}

void
gnatcov_rts_reset_buffers (const struct gnatcov_rts_coverage_buffers *buffs)
{
  if (buffs == NULL)
    return;

  if (buffs->statement != NULL)
    memset (buffs->statement, 0, buffs->statement_last_bit + 1);
  if (buffs->decision != NULL)
    memset (buffs->decision, 0, buffs->decision_last_bit + 1);
  if (buffs->mcdc != NULL)
    memset (buffs->mcdc, 0, buffs->mcdc_last_bit + 1);
}

void
gnatcov_rts_reset_buffer_group (
  const struct gnatcov_rts_coverage_buffers_group *group)
{
  if (group == NULL)
    return;

  unsigned i;
  for (i = 0; i < group->length; i++)
    gnatcov_rts_reset_buffers (group->buffers[i]);
}

void
gnatcov_rts_reset_group_array (
  const struct gnatcov_rts_coverage_buffers_group_array *arr)
{
  if (arr == NULL)
    return;

  unsigned i;
  for (i = 0; i < arr->length; i++)
    gnatcov_rts_reset_buffer_group (arr->groups[i]);
}
