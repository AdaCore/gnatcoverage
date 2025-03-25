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

    /* Hash of annotations for this unit, as gnatcov computes it (see
       SC_Obligations). Used as a fast way to check that source traces and
       coverage data are consistent.  */
    uint8_t annotations_fingerprint[FINGERPRINT_SIZE];

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

  /*****************/
  /* Observability */
  /*****************/

  /* Return the sum of all bits in the given buffers.
     A higher number means a higher coverage. */
  uint64_t gnatcov_rts_sum_buffer_bits (
    const struct gnatcov_rts_coverage_buffers_group_array *arr);

  /*********************/
  /* Clear subprograms */
  /*********************/

  /* The following subprograms may be called to clear coverage buffers,
     and thus reset the coverage execution information accumulated.  */

  /* Clear the buffers for a single unit.  */
  void
  gnatcov_rts_reset_buffers (const struct gnatcov_rts_coverage_buffers *buffs);

  /* Clear the buffers for a whole buffer group.  */
  void gnatcov_rts_reset_buffer_group (
    const struct gnatcov_rts_coverage_buffers_group *group);

  /* Set all the values in all the buffers of each group denoted by arr to
     zero. This clears the buffers of all coverage levels.  */
  extern void gnatcov_rts_reset_group_array (
    const struct gnatcov_rts_coverage_buffers_group_array *arr);

#ifdef __cplusplus
}
#endif

#endif
