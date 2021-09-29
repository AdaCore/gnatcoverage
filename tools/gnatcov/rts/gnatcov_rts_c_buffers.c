/****************************************************************************
 *                                                                          *
 *                   GNATcoverage Instrumentation Runtime                   *
 *                                                                          *
 *                     Copyright (C) 2021-2021, AdaCore                     *
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

#include "gnatcov_rts_c_buffers.h"

unsigned
gnatcov_rts_witness (void *buffer_address, unsigned bit_id)
{
  uint8_t *buffer = (uint8_t *)buffer_address;
  buffer[bit_id] = (uint8_t)1;
  return 1;
}

unsigned
gnatcov_rts_witness_decision (void *buffer_address, unsigned false_bit,
                              unsigned true_bit, unsigned value)
{
  if (value)
    gnatcov_rts_witness (buffer_address, true_bit);
  else
    gnatcov_rts_witness (buffer_address, false_bit);
  return value;
}

unsigned
gnatcov_rts_witness_decision_mcdc (void *decision_buffer_address,
                                   unsigned false_bit, unsigned true_bit,
                                   void *mcdc_buffer_address,
                                   unsigned mcdc_base, void *mcdc_path_address,
                                   unsigned value)
{
  unsigned mcdc_path_index = *((unsigned *)mcdc_path_address);
  gnatcov_rts_witness (mcdc_buffer_address, mcdc_base + mcdc_path_index);
  return gnatcov_rts_witness_decision (decision_buffer_address, false_bit,
                                       true_bit, value);
}

unsigned
gnatcov_rts_witness_condition (unsigned *mcdc_path_address,
                               unsigned offset_for_true, unsigned first,
                               unsigned value)
{
  if (first)
    *mcdc_path_address = 0;
  if (value)
    *mcdc_path_address += offset_for_true;
  return value;
}
