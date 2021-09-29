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

#include <stddef.h>
#include <stdint.h>

#define FINGERPRINT_SIZE 20

extern unsigned gnatcov_rts_witness (void *buffer_address, unsigned bit_id);

extern unsigned gnatcov_rts_witness_decision (void *buffer_address,
                                              unsigned false_bit,
                                              unsigned true_bit,
                                              unsigned value);

extern unsigned gnatcov_rts_witness_decision_mcdc (
    void *decision_buffer_address, unsigned false_bit, unsigned true_bit,
    void *mcdc_buffer_address, unsigned mcdc_base, void *mcdc_path_address,
    unsigned value);

extern unsigned gnatcov_rts_witness_condition (unsigned *mcdc_path_address,
                                               unsigned offset_for_true,
                                               unsigned first, unsigned value);

typedef struct gnatcov_rts_unit_coverage_buffers
{
  char fingerprint[FINGERPRINT_SIZE];
  int language;
  int unit_part;
  char *unit_name;
  char *project_name;
  size_t unit_name_length;
  size_t project_name_length;
  uint8_t *statement, *decision, *mcdc;
  unsigned statement_last_bit, decision_last_bit, mcdc_last_bit;
} gnatcov_rts_unit_coverage_buffers;

typedef struct gnatcov_rts_unit_coverage_buffers_array
{
  size_t length;
  gnatcov_rts_unit_coverage_buffers **buffers;
} gnatcov_rts_unit_coverage_buffers_array;
