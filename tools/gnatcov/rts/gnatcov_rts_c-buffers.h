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

#ifndef GNATCOV_RTS_C_BUFFERS_H
#define GNATCOV_RTS_C_BUFFERS_H

#include <stdint.h>

#include "gnatcov_rts_c-strings.h"

#ifdef __cplusplus
extern "C"
{
#endif

  /* Coverage buffers are big arrays of booleans. How to interpret them depends
     on the type of coverage obligation.

     For statement coverage, each statement is assigned such a boolean, which
     indicates whether it was executed.

     For decision coverage, each decision is assigned two booleans: one which
     indicates whether the decision reached the False outcome and another for
     the True outcome.

     For MC/DC coverage, there is a boolean for each decision valuation path in
     the binary decision diagram.  */

  /* Representation of the boolean type.  */
  typedef unsigned gnatcov_rts_bool;

  /* Unique identifier for a boolean in a coverage buffer.  */
  typedef int gnatcov_rts_bit_id;

#define FINGERPRINT_SIZE 20

  enum gnatcov_rts_unit_part
  {
    /* `NOT_APPLICABLE_PART` is a default value used for compilation units in
       languages that are not unit-based.  */
    NOT_APPLICABLE_PART = 0,
    UNIT_BODY = 1,
    UNIT_SPEC = 2,
    UNIT_SEPARATE = 3
  };

  enum gnatcov_rts_language_kind
  {
    UNIT_BASED_LANGUAGE = 0,
    FILE_BASED_LANGUAGE = 1
  };

  struct gnatcov_rts_coverage_buffers
  {
    /* Hash of SCO info for this unit, as gnatcov computes it (see
       SC_Obligations).  Used as a fast way to check that coverage obligations
       and coverage data are consistent. Specific hash values are computed
       during instrumentation.  */
    uint8_t fingerprint[FINGERPRINT_SIZE];

    /* Language kind for this unit.  */
    enum gnatcov_rts_language_kind language_kind;

    /* Unit kind and name for the instrumented unit.  The unit_name field
       accounts both for unit-based languages (such as Ada) and file-based
       languages such as C.

       The unit_part field is only there for unit-based languages and is set
       to NOT_APPLICABLE_PART for file-based languages.

       More specifically, for unit-based languages, unit_name is the fully
       qualified name of the compilation unit (or subunit) in lower case.
       For instance: "foo", "ada.text_io" or "foo.bar.my_subunit".

       For file-based languages, unit_name is the simple filename, e.g.
       "foo.c".  */
    enum gnatcov_rts_unit_part unit_part;
    struct gnatcov_rts_string unit_name;

    /* Hash of buffer bit mappings for this unit, as gnatcov computes it (see
       SC_Obligations).  Used as a fast way to check that gnatcov will be able
       to interpret buffer bits from a source traces using buffer bit mappings
       from SID files.  */
    uint8_t bit_maps_fingerprint[FINGERPRINT_SIZE];

    /* Pointers to coverage buffers for statement obligations, decision
       obligations and MC/DC obligations.  The size of each array is in the
       corresponding *_last_bit field.  */
    uint8_t *statement, *decision, *mcdc;

    /* Index for the last bits in coverage buffers for statements, decisions
       and MC/DC.  */
    gnatcov_rts_bit_id statement_last_bit, decision_last_bit, mcdc_last_bit;
  };

  /* Group of coverage buffers.  For a given source file, instrumentation may
     create more than one set of coverage buffers: these are grouped in a
     coverage buffers group.  */
  struct gnatcov_rts_coverage_buffers_group
  {
    unsigned length;
    const struct gnatcov_rts_coverage_buffers **buffers;
  };

  /* Array of coverage buffers groups.  */
  struct gnatcov_rts_coverage_buffers_group_array
  {
    unsigned length;
    const struct gnatcov_rts_coverage_buffers_group **groups;
  };

  /***********************/
  /* Witness subprograms */
  /***********************/

  /* The following subprograms are called by generated code to record
     the execution of constructs.  */

  /* Statements */

  /* Set the boolean corresponding to BIT to true in BUFFER in various
     context.  */
  extern gnatcov_rts_bool gnatcov_rts_witness (uint8_t *buffer,
					       gnatcov_rts_bit_id bit);

  /* Decisions */

  /* If VALUE is false, set the boolean corresponding to FALSE_BIT to true in
     BUFFER.  Set the one corresponding to TRUE_BIT otherwise.  */
  extern gnatcov_rts_bool
  gnatcov_rts_witness_decision (uint8_t *buffer, gnatcov_rts_bit_id false_bit,
				gnatcov_rts_bit_id true_bit,
				gnatcov_rts_bool value);

  /* Same as above, and also set the bit determined by MCDC_BASE and the
     gnatcov_rts_bit_id value at MCDC_PATH_ADDRESS in the buffer at
     MCDC_BUFFER_ADDRESS.  Note that MCDC_PATH may not be passed by value,
     because it is not known until the side effect of the actual for the VALUE
     formal have been evaluated.  */
  extern gnatcov_rts_bool gnatcov_rts_witness_decision_mcdc (
    uint8_t *decision_buffer, gnatcov_rts_bit_id false_bit,
    gnatcov_rts_bit_id true_bit, uint8_t *mcdc_buffer,
    gnatcov_rts_bit_id mcdc_base, gnatcov_rts_bit_id *mcdc_path_address,
    gnatcov_rts_bool value);

  /* Conditions */

  /* MCDC_PATH_ADDRESS is the address of a local variable storing the
     mcdc_state. If FIRST is true, first reset it to 0.  If VALUE is true, add
     OFFSET_FOR_TRUE.  */
  extern gnatcov_rts_bool gnatcov_rts_witness_condition (
    gnatcov_rts_bit_id *mcdc_path_address, gnatcov_rts_bit_id offset_for_true,
    gnatcov_rts_bool first, gnatcov_rts_bool value);

#ifdef __cplusplus
}
#endif

#endif
