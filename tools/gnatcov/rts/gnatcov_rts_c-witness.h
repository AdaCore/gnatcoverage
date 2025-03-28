/****************************************************************************
 *                                                                          *
 *                   GNATcoverage Instrumentation Runtime                   *
 *                                                                          *
 *                       Copyright (C) 2024, AdaCore                        *
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

/***********************/
/* Witness subprograms */
/***********************/

/* The following subprograms are called by generated code to record
   the execution of constructs.  */

/* Statements */

/* Set the boolean corresponding to BIT to true in BUFFER in various
   context.  */
static inline int
gnatcov_rts_witness (unsigned char *buffer, unsigned bit_id)
{
  buffer[bit_id] = 1;
  return 1;
}

#ifdef __cplusplus

template <class T>
T
gnatcov_rts_witness_generic (unsigned char *buffer, unsigned int bit_id,
                             T value)
{
  gnatcov_rts_witness (buffer, bit_id);
  return value;
}

#endif /* __cplusplus */

/* Decisions */

/* If VALUE is false, set the boolean corresponding to FALSE_BIT to true in
   BUFFER.  Set the one corresponding to TRUE_BIT otherwise.  */
static inline int
gnatcov_rts_witness_decision (unsigned char *buffer, unsigned false_bit,
                              unsigned true_bit, int value)
{
  gnatcov_rts_witness (buffer, value ? true_bit : false_bit);
  return value;
}

/* Same as above, and also set the bit determined by MCDC_BASE and the
   gnatcov_rts_bit_id value at MCDC_PATH_ADDRESS in the buffer at
   MCDC_BUFFER_ADDRESS.  Note that MCDC_PATH may not be passed by value,
   because it is not known until the side effect of the actual for the VALUE
   formal have been evaluated.  */
static inline int
gnatcov_rts_witness_decision_mcdc (unsigned char *decision_buffer,
                                   unsigned false_bit, unsigned true_bit,
                                   unsigned char *mcdc_buffer,
                                   unsigned mcdc_base,
                                   unsigned *mcdc_path_address, int value)
{
  unsigned mcdc_path_index = *mcdc_path_address;
  gnatcov_rts_witness (mcdc_buffer, mcdc_base + mcdc_path_index);
  return gnatcov_rts_witness_decision (decision_buffer, false_bit, true_bit,
                                       value);
}

/* Conditions */

/* MCDC_PATH_ADDRESS is the address of a local variable storing the
   mcdc_state. If FIRST is true, first reset it to 0.  If VALUE is true, add
   OFFSET_FOR_TRUE.  */
static inline int
gnatcov_rts_witness_condition (unsigned *mcdc_path_address,
                               unsigned offset_for_true, unsigned first,
                               int value)
{
  unsigned *mcdc_path_index = mcdc_path_address;
  if (first)
    *mcdc_path_index = 0;
  if (value)
    *mcdc_path_index += offset_for_true;
  return value;
}
