/****************************************************************************
 *                                                                          *
 *                   GNATcoverage Instrumentation Runtime                   *
 *                                                                          *
 *                     Copyright (C) 2021-2023, AdaCore                     *
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

#ifndef GNATCOV_RTS_C_TRACES_H
#define GNATCOV_RTS_C_TRACES_H

#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

/* Expected value of the `trace_file_header.format_version` field.

   0 -- initial version
   1 -- extend trace entry model to account for C files
   2 -- introduce fingerprints for bit maps  */
#define GNATCOV_RTS_CURRENT_VERSION 2

/* LSB_First_Bytes: bit buffers are encoded as sequences of bytes.

   * Booleans are encoded with bits the usual way: 0 for False and 1 for
     True.

   * Byte X contains bits for bits 8 * Y to 8 * Y + 7.  For instance, byte
     1 contains bits 8 to 15.

   * Inside byte X, least significant bit maps to bit 8 * Y while the most
     significant bit maps to bit 8 * Y + 7.  */
#define GNATCOV_RTS_LSB_FIRST_BYTES 0

  enum
  {
    GNATCOV_RTS_LITTLE_ENDIAN,
    GNATCOV_RTS_BIG_ENDIAN
  };

  /* Return the native endianity, 0 for little-endian, 1 for big-endian.  */
  extern unsigned gnatcov_rts_native_endianity (void);

  struct trace_file_header
  {
    char magic[32];

    uint32_t format_version;
    uint8_t alignment;
    uint8_t endianity;

    /* Padding used only to make the size of the trace file header a
       multiple of 8 bytes.  Must be zero.  */
    uint16_t padding;
  };

  /*********************/
  /* Trace information */
  /*********************/

  enum
  {

    /* Special trace info entry: indicates the end of a sequence of entries. No
       data is associated to this trace info entry.  */
    GNATCOV_RTS_INFO_END = 0,

    /* Name of the program that produced this trace.  */
    GNATCOV_RTS_INFO_PROGRAM_NAME = 1,

    /* Date for the program execution that produced this trace.  */
    GNATCOV_RTS_INFO_EXEC_DATE = 2,

    /* Arbitrary storage for user data. This is exposed to users as the trace
       "tag".  */
    GNATCOV_RTS_INFO_USER_DATA = 3
  };

  struct trace_info_header
  {
    uint32_t kind;
    uint32_t length;
  };

  /* Trace entry header

     Each trace entry starts with the following header. Then goes:

     * The name of the unit describes. It is NUL-padded according to the trace
       file alignment.

     * The statement coverage buffer. It is also NUL-padded.

     * The decision coverage buffer. It is also NUL-padded.

     * The MC/DC coverage buffer. It is also NUL-padded.  */

  struct trace_entry_header
  {
    /* Length of the unit name / filename for the unit this trace entry
       describes.  */
    uint32_t unit_name_length;

    /* For file-based languages, length of the project name this file belongs
       to. For unit-based languages, the unit name is unique so this piece of
       information is not needed (and thus will be 0).  */
    uint32_t project_name_length;

    /* Number of bits in the statement, decision and MC/DC coverage buffers. */
    uint32_t statement_bit_count;
    uint32_t decision_bit_count;
    uint32_t mcdc_bit_count;

    /* Language kind for this unit */
    uint8_t language_kind;

    /* Part of the unit this trace entry describes. `not_applicable_part` for
       file-based languages.  */
    uint8_t unit_part;

    /* Encoding used to represent statement and decision coverage buffers.  */
    uint8_t bit_buffer_encoding;

    /* Hash of SCO info for this unit. Used as a fast way to check that
       coverage obligations and coverage data are consistent. Specific hash
       values are computed during instrumentation.  */
    uint8_t fingerprint[20];

    /* Hash of buffer bit mappings for this unit, as gnatcov computes it (see
       SC_Obligations).  Used as a fast way to check that gnatcov will be able
       to interpret buffer bits from a source traces using buffer bit mappings
       from SID files.  */
    uint8_t bit_maps_fingerprint[20];

    /* Padding used only to make the size of this trace entry header a multiple
       of 8 bytes. Must be zero.  */
    uint8_t padding[1];
  };

#ifdef __cplusplus
}
#endif

#endif
