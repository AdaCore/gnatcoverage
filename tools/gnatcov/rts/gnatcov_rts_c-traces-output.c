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

#include "gnatcov_rts_c-traces-output.h"
#include "gnatcov_rts_c-traces.h"
#include <stdint.h>

static const unsigned alignment = sizeof (void *);

struct info_entry
{
  const void *data;
  uint32_t length;
};

/* Write `count` padding bytes.  */
static void
write_padding (gnatcov_rts_write_bytes_callback write_bytes, void *output,
	       unsigned count)
{
  unsigned pad_count = (alignment - count) % alignment;
  if (pad_count != alignment)
    {
      uint8_t bytes[pad_count];
      memset (bytes, 0, pad_count);
      write_bytes (output, bytes, pad_count);
    }
}

/* Write the header of a source trace file.  */
static void
write_header (gnatcov_rts_write_bytes_callback write_bytes, void *output)
{
  struct trace_file_header header;
  memset (header.magic, 0, 32);
  memcpy (header.magic, "GNATcov source trace file", 25);
  header.format_version = GNATCOV_RTS_CURRENT_VERSION;
  header.alignment = alignment;
  header.endianity = gnatcov_rts_native_endianity ();
  header.padding = 0;

  write_bytes (output, (char *) &header, sizeof (struct trace_file_header));
}

/* Write an information entry (e.g. the program_name, the exec date etc.).  */
static void
write_info (gnatcov_rts_write_bytes_callback write_bytes, void *output,
	    uint32_t kind, struct info_entry *data)
{
  struct trace_info_header header;
  header.kind = kind;
  header.length = data->length;
  write_bytes (output, &header, sizeof (header));
  write_bytes (output, data->data, data->length);
  write_padding (write_bytes, output, data->length);
}

/* Write the `current_byte` and increase the `bytes_count`. If `bit_mask` is 1,
   it means that the `current_byte` contains no data (and thus there is no need
   to write anything).  */
static void
flush (gnatcov_rts_write_bytes_callback write_bytes, void *output,
       uint8_t *bit_mask, uint8_t *current_byte, unsigned *bytes_count)
{
  if (*bit_mask != 1)
    {
      write_bytes (output, current_byte, 1);
      *current_byte = 0;
      *bit_mask = 1;
      (*bytes_count)++;
    }
}

/* Write a coverage bit buffer.  */
static void
write_buffer (gnatcov_rts_write_bytes_callback write_bytes, void *output,
	      uint8_t *buffer, unsigned buffer_length)
{
  uint8_t current_byte = 0;
  uint8_t bit_mask = 1;
  unsigned bytes_count = 0;
  unsigned i;

  for (i = 0; i < buffer_length; i++)
    {
      if (buffer[i])
	current_byte = current_byte | bit_mask;
      if (bit_mask == 1 << 7)
	flush (write_bytes, output, &bit_mask, &current_byte, &bytes_count);
      else
	bit_mask <<= 1;
    }
  flush (write_bytes, output, &bit_mask, &current_byte, &bytes_count);

  write_padding (write_bytes, output, bytes_count);
}

/* Write a coverage buffer (unit information + coverage bit buffers).  */
static void
write_entry (gnatcov_rts_write_bytes_callback write_bytes, void *output,
	     const struct gnatcov_rts_coverage_buffers *buffers)
{
  struct trace_entry_header header;
  header.unit_name_length = (uint32_t) buffers->unit_name.length;
  header.statement_bit_count = (uint32_t) buffers->statement_last_bit + 1;
  header.decision_bit_count = (uint32_t) buffers->decision_last_bit + 1;
  header.mcdc_bit_count = (uint32_t) buffers->mcdc_last_bit + 1;
  header.language_kind = (uint8_t) buffers->language_kind;
  header.unit_part = (uint8_t) buffers->unit_part;
  header.bit_buffer_encoding = GNATCOV_RTS_LSB_FIRST_BYTES;
  memset (header.padding, 0, 5);
  memcpy (&header.fingerprint, buffers->fingerprint, FINGERPRINT_SIZE);
  memcpy (&header.bit_maps_fingerprint, buffers->bit_maps_fingerprint,
	  FINGERPRINT_SIZE);

  write_bytes (output, (char *) &header, sizeof (header));
  write_bytes (output, buffers->unit_name.str, buffers->unit_name.length);
  write_padding (write_bytes, output, buffers->unit_name.length);
  write_buffer (write_bytes, output, buffers->statement,
		buffers->statement_last_bit + 1);
  write_buffer (write_bytes, output, buffers->decision,
		buffers->decision_last_bit + 1);
  write_buffer (write_bytes, output, buffers->mcdc,
		buffers->mcdc_last_bit + 1);
}

/* Write a uint64_t timestamp (by splitting it in uint8_t chunks).  */
static void
write_date (gnatcov_rts_write_bytes_callback write_bytes, void *output,
	    uint64_t timestamp)
{
  uint8_t formatted_date[8];
  struct info_entry entry;
  int i;

  for (i = 0; i < 8; i++)
    {
      formatted_date[i] = timestamp & 0xFF;
      timestamp >>= 8;
    }

  entry.data = &formatted_date[0];
  entry.length = 8;
  write_info (write_bytes, output, GNATCOV_RTS_INFO_EXEC_DATE, &entry);
}

/* See gnatcov_rts_c-traces-output.h.  */
int
gnatcov_rts_generic_write_trace_file (
  void *output,
  const struct gnatcov_rts_coverage_buffers_group_array *buffers_groups,
  struct gnatcov_rts_string program_name, uint64_t exec_date,
  struct gnatcov_rts_string user_data,
  gnatcov_rts_write_bytes_callback write_bytes)
{
  const struct gnatcov_rts_coverage_buffers_group *group;
  unsigned i_group, i_buffer;

  struct info_entry program_name_entry;
  program_name_entry.length = program_name.length;
  program_name_entry.data = program_name.str;

  struct info_entry user_data_entry;
  user_data_entry.length = user_data.length;
  user_data_entry.data = user_data.str;

  struct info_entry end_entry;
  end_entry.length = 0;

  write_header (write_bytes, output);
  write_info (write_bytes, output, GNATCOV_RTS_INFO_PROGRAM_NAME,
	      &program_name_entry);
  write_date (write_bytes, output, exec_date);
  write_info (write_bytes, output, GNATCOV_RTS_INFO_USER_DATA,
	      &user_data_entry);
  write_info (write_bytes, output, GNATCOV_RTS_INFO_END, &end_entry);

  for (i_group = 0; i_group < buffers_groups->length; ++i_group)
    {
      group = buffers_groups->groups[i_group];
      for (i_buffer = 0; i_buffer < group->length; ++i_buffer)
	write_entry (write_bytes, output, group->buffers[i_buffer]);
    }

  return 0;
}
