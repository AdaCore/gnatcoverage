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
#include <stddef.h>
#include <stdint.h>

gnatcov_rts_bool
gnatcov_rts_witness (uint8_t *buffer, gnatcov_rts_bit_id bit)
{
  buffer[bit] = 1;
  return 1;
}

gnatcov_rts_bool
gnatcov_rts_witness_decision (uint8_t *buffer, gnatcov_rts_bit_id false_bit,
                              gnatcov_rts_bit_id true_bit,
                              gnatcov_rts_bool value)
{
  gnatcov_rts_witness (buffer, value ? true_bit : false_bit);
  return value;
}

gnatcov_rts_bool
gnatcov_rts_witness_decision_mcdc (uint8_t *decision_buffer,
                                   gnatcov_rts_bit_id false_bit,
                                   gnatcov_rts_bit_id true_bit,
                                   uint8_t *mcdc_buffer,
                                   gnatcov_rts_bit_id mcdc_base,
                                   gnatcov_rts_bit_id *mcdc_path_address,
                                   gnatcov_rts_bool value)
{
  gnatcov_rts_bit_id mcdc_path_index = *mcdc_path_address;
  gnatcov_rts_witness (mcdc_buffer, mcdc_base + mcdc_path_index);
  return gnatcov_rts_witness_decision (decision_buffer, false_bit, true_bit,
                                       value);
}

gnatcov_rts_bool
gnatcov_rts_witness_condition (gnatcov_rts_bit_id *mcdc_path_address,
                               gnatcov_rts_bit_id offset_for_true,
                               gnatcov_rts_bool first, gnatcov_rts_bool value)
{
  gnatcov_rts_bit_id *mcdc_path_index
    = (gnatcov_rts_bit_id *) mcdc_path_address;
  if (first)
    *mcdc_path_index = 0;
  if (value)
    *mcdc_path_index += offset_for_true;
  return value;
}
